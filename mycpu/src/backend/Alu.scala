package backend
import config._
import backend._
import bundle._
import chisel3._
import utils._
import chisel3.util.Cat
import chisel3.util.MuxCase
import chisel3.util.switch

class Adder extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val src1 = Input(UWord)
      val src2 = Input(UWord)
      val op   = Input(Vec(4, Bool()))
    }
    val out = new Bundle {
      val res      = Output(UWord)
      val overflow = Output(Bool())
    }
  })
  /*====================  op  ====================*/
  val (src1, src2, op) = (io.in.src1, io.in.src2, io.in.op)
  assert(io.in.op.length == 4)
  val useAdd  = op(3)
  val useSub  = op(2)
  val useSlt  = op(1)
  val useSltu = op(0)
  /*====================Function Code====================*/
  val cin   = Mux((useSub | useSlt | useSltu), 1.U, 0.U)
  val dataA = src1
  val dataB = Mux((useSub | useSlt | useSltu), ~src2, src2)
  val topA  = Mux(useSltu, 0.U(1.W), dataA(31))
  val topB  = Mux(useSltu, 0.U(1.W), dataB(31))
  val res33 = Wire(UInt(33.W))
  asg(res33, Cat(0.U(1.W), dataA) + Cat(0.U(1.W), dataB) + cin)
  val carry     = res33(32)
  val addSubRes = res33(31, 0)
  val cout      = topB + topA + carry
  asg(io.out.overflow, cout ^ addSubRes(31))

  val sltRes  = Cat(0.U(31.W), addSubRes(31) ^ io.out.overflow)
  val sltuRes = Cat(0.U(31.W), Mux(cout === 1.U, 0.U(1.W), 1.U(1.W)))
  asg(
    io.out.res,
    MuxCase(
      0.U(dataWidth.W),
      Seq(
        (useAdd || useSub) -> addSubRes,
        useSlt             -> sltRes,
        useSltu            -> sltuRes
      )
    )
  )
  /*==================== Access Code ====================*/
  def access(src1: UInt, src2: UInt, op: Vec[Bool]) = {
    asg(io.in.src1, src1)
    asg(io.in.src2, src2)
    asg(io.in.op, op)
    io.out
  }
}

class AluComponent extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val src1 = Input(UWord)
      val src2 = Input(UWord)
      val op   = Input(AluType())
    }
    val out = new Bundle {
      val overflow = Output(Bool())
      val res      = Output(UWord)
    }
  })
  /*====================  op  ====================*/
  val (src1, src2, op) = (io.in.src1, io.in.src2, io.in.op)
  val useAdd           = AluType.useAdd(op)
  val useSub           = AluType.useSub(op)
  val useAnd           = AluType.useAnd(op)
  val useOr            = AluType.useOr(op)
  val useNor           = AluType.useNor(op)
  val useXor           = AluType.useXor(op)
  val useSlt           = AluType.useSlt(op)
  val useSltu          = AluType.useSltu(op)
  val useSll           = AluType.useSll(op)
  val useSrl           = AluType.useSrl(op)
  val useSra           = AluType.useSra(op)
  val useLui           = AluType.useLui(op)
  val mayOfInst        = AluType.mayOverflow(op)
  /*====================Function Code====================*/
  //与、或、或非、异或、逻辑左移右移、算数右移、高位置数
  val andRes = src1 & src2
  val orRes  = src1 | src2
  val norRes = ~(src1 | src2)
  val xorRes = src1 ^ src2
  val sllRes = (src2 << src1(4, 0))(31, 0) //UInt左移会使位宽增加
  val srlRes = UWord
  srlRes := src2 >> src1(4, 0) //TODO:
  val sraRes = (src2.asSInt >> src1(4, 0)).asUInt
  val luiRes = Cat(src2(15, 0), 0.U(16.W))
  //加减、无符号比较、有符号比较
  val adder    = Module(new Adder)
  val adderOp  = VecInit(useAdd, useSub, useSlt, useSltu)
  val adderOut = adder.access(src1, src2, adderOp)
  //整理计算结果
  asg(io.out.overflow, adderOut.overflow && mayOfInst)
  asg(
    io.out.res,
    MuxCase(
      0.U(dataWidth.W),
      Seq(
        adderOp.asUInt.orR -> adderOut.res,
        useAnd             -> andRes,
        useOr              -> orRes,
        useNor             -> norRes,
        useXor             -> xorRes,
        useSll             -> sllRes,
        useSrl             -> srlRes,
        useSra             -> sraRes,
        useLui             -> luiRes
      )
    )
  )
  /*==================== Access Code ====================*/
  def access(src1: UInt, src2: UInt, op: AluType.Type) = {
    asg(io.in.src1, src1)
    asg(io.in.src2, src2)
    asg(io.in.op, op)
    io.out
  }
}

class BrCondGen extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val src1 = Input(UWord)
      val src2 = Input(UWord)
      val op   = Input(BranchType())
    }
    val out = new Bundle {
      val taken = Output(Bool())
    }
  })
  /*====================  op  ====================*/
  val (src1, src2, op) = (io.in.src1, io.in.src2, io.in.op)
  val eq               = BranchType.eq(op)
  val ne               = BranchType.ne(op)
  val gez              = BranchType.gez(op)
  val gtz              = BranchType.gtz(op)
  val lez              = BranchType.lez(op)
  val ltz              = BranchType.ltz(op)
  val noCond           = BranchType.noCond(op)
  /*====================  select cond  ====================*/
  val negtive = src1(31)
  val zero    = src1 === 0.U(32.W)
  asg(
    io.out.taken,
    MuxCase(
      false.B,
      Seq(
        eq     -> (src1 === src2),
        ne     -> (src1 =/= src2),
        gez    -> !negtive,
        lez    -> (negtive || zero),
        ltz    -> negtive,
        gtz    -> !(negtive || zero),
        noCond -> true.B
      )
    )
  )
  /*==================== Access Code ====================*/
  def access(src1: UInt, src2: UInt, op: BranchType.Type) = {
    asg(io.in.src1, src1)
    asg(io.in.src2, src2)
    asg(io.in.op, op)
    io.out.taken
  }
}

class Alu(main: Boolean) extends FuncUnit(FuType.MainAlu) {
  val extInt = if (main) Some(IO(UInt(6.W))) else None

  //stage connect
  val exeStageIO = new ExeStageIO(FuType.MainAlu)
  exeStageIO.out <> io.out
  PipelineConnect(roStage.io.out, exeStageIO.in, exeStageIO.out.fire, io.flush)
  val exeIn  = exeStageIO.in.bits
  val exeOut = exeStageIO.out.bits

  //注意，这里的in.valid已经代表着pipex_valid
  val exeStageReadyGo = true.B
  exeStageIO.out.valid := exeStageIO.in.valid && exeStageReadyGo
  exeStageIO.in.ready  := !exeStageIO.in.valid || exeStageIO.out.ready && exeStageReadyGo

  //simple connect
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wPrf.wmask, 15.U(4.W))
  asg(exeOut.wbRob.takeWord, exeIn.srcData(1)) //only mtc0 care
  asg(exeOut.wbRob.isMispredict, false.B) //default,mainAlu may change it
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)

  /**
    * real alu logic:mainAlu/SubAlu
    *
    * gen:
    *   1.wprf.result
    *   2.wRob.exception
    *   3.wRob.isMispredict(for mainAlu)
    */
  val srcs    = exeIn.srcData
  val uOp     = exeIn.decoded
  val aluType = uOp.aluType
  val brType  = uOp.brType

  //alu and overflow
  val aluComponent = Module(new AluComponent)
  val aluCpOut     = aluComponent.access(srcs(0), srcs(1), aluType)
  asg(exeOut.wPrf.result, aluCpOut.res) //default,in mAlu may change it("AL")

  //bru
  if (main) {
    val brCondGen = Module(new BrCondGen)
    val taken     = brCondGen.access(srcs(0), srcs(1), brType)

  }
}
