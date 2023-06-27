package backend
import config._
import backend._
import bundle._
import chisel3._
import utils._
import chisel3.util.Cat
import chisel3.util.MuxCase

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
  val isAdd  = op(3)
  val isSub  = op(2)
  val isSlt  = op(1)
  val isSltu = op(0)
  /*====================Function Code====================*/
  val cin   = Mux((isSub | isSlt | isSltu), 1.U, 0.U)
  val dataA = src1
  val dataB = Mux((isSub | isSlt | isSltu), ~src2, src2)
  val topA  = Mux(isSltu, 0.U(1.W), dataA(31))
  val topB  = Mux(isSltu, 0.U(1.W), dataB(31))
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
        (isAdd || isSub) -> addSubRes,
        isSlt            -> sltRes,
        isSltu           -> sltuRes
      )
    )
  )
  /*====================Access Code====================*/
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
      val mayOverflow = Output(Bool())
      val res         = Output(UWord)
    }
  })
  /*====================  op  ====================*/
  val (src1, src2, op) = (io.in.src1, io.in.src2, io.in.op)
  val isAdd            = AluType.isAdd(op)
  val isSub            = AluType.isSub(op)
  val isAnd            = AluType.isAnd(op)
  val isOr             = AluType.isOr(op)
  val isNor            = AluType.isNor(op)
  val isXor            = AluType.isXor(op)
  val isSlt            = AluType.isSlt(op)
  val isSltu           = AluType.isSltu(op)
  val isSll            = AluType.isSll(op) || AluType.isSllv(op)
  val isSrl            = AluType.isSrl(op) || AluType.isSrlv(op)
  val isSra            = AluType.isSra(op) || AluType.isSrav(op)
  val isLui            = AluType.isLui(op)
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
  val adderOp  = VecInit(isAdd, isSub, isSlt, isSltu)
  val adderOut = adder.access(src1, src2, adderOp)
  //整理计算结果
  asg(io.out.mayOverflow, adderOut.overflow)
  asg(
    io.out.res,
    MuxCase(
      0.U(dataWidth.W),
      Seq(
        adderOp.asUInt.orR -> adderOut.res,
        isAnd              -> andRes,
        isOr               -> orRes,
        isNor              -> norRes,
        isXor              -> xorRes,
        isSll              -> sllRes,
        isSrl              -> srlRes,
        isSra              -> sraRes,
        isLui              -> luiRes
      )
    )
  )
  /*====================Access Code====================*/
  def access(src1: UInt, src2: UInt, op: AluType.Type) = {
    asg(io.in.src1, src1)
    asg(io.in.src2, src2)
    asg(io.in.op, op)
    io.out
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

  //unchange signal
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
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
  val srcs = exeIn.srcData
  //...
}
