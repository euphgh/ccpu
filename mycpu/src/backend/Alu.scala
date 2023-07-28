package backend
import config._
import backend._
import bundle._
import chisel3._
import utils._

import chisel3.util.Cat
import chisel3.util.MuxCase
import chisel3.util.switch
import chisel3.util.experimental.BoringUtils
import frontend.BpuUpdateIO
import frontend.MispreSignal
import chisel3.util.Valid
import difftest.DifftestBackPred
import frontend.PatternHistoryTable
import frontend.RetAddrStack

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
  val useAdd  = op(0)
  val useSub  = op(1)
  val useSlt  = op(2)
  val useSltu = op(3)
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
  val srlRes = Wire(UWord)
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

//gen cond
class BrHandler extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val src1 = Input(UWord)
      val src2 = Input(UWord)
      val op   = Input(BranchType())
    }
    val out = new Bundle {
      val taken = Output(Bool())
    }
    val rtEqZero = Output(Bool())
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
  asg(io.rtEqZero, src2 === 0.U(32.W))
  /*==================== Access Code ====================*/
  def access(src1: UInt, src2: UInt, op: BranchType.Type) = {
    asg(io.in.src1, src1)
    asg(io.in.src2, src2)
    asg(io.in.op, op)
    io.out.taken
  }
}

class Alu(main: Boolean) extends FuncUnit(if (main) FuType.MainAlu else FuType.SubAlu) {

  val bpuUpdate = if (main) Some(IO(new BpuUpdateIO)) else None
  val mispre    = if (main) Some(IO(new MispreSignal)) else None

  //stage connect
  val exeStageIO = Wire(new ExeStageIO(if (main) FuType.MainAlu else FuType.SubAlu))
  exeStageIO.out <> io.out
  PipelineConnect(roStage.io.out, exeStageIO.in, exeStageIO.out.fire, io.flush)
  val exeIn     = exeStageIO.in.bits
  val exeOut    = exeStageIO.out.bits
  val instValid = exeStageIO.in.valid

  //注意，这里的in.valid已经代表着pipex_valid
  val exeStageReadyGo = true.B
  exeStageIO.out.valid := exeStageIO.in.valid && exeStageReadyGo
  exeStageIO.in.ready  := !exeStageIO.in.valid || exeStageIO.out.ready && exeStageReadyGo

  //unchange signal
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wPrf.wmask, 15.U(4.W))
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)
  if (debug) asg(exeOut.wbRob.debugPC.get, exeIn.debugPC.get)

  //may change signal
  val outExDetect = exeOut.wbRob.exDetect
  val inExDetect  = exeIn.exDetect
  asg(outExDetect, inExDetect) //when exception occur,may change it
  asg(exeOut.wbRob.isMispredict, false.B) //mainAlu may change it

  /**
    * real alu logic:mainAlu/SubAlu
    *
    * gen:
    *   1.wprf.result
    *   2.wRob.exception
    *   3.wRob.isMispredict(for mainAlu)
    */
  val srcs    = exeIn.srcData
  val uOp     = exeIn.uOp
  val aluType = uOp.aluType.get

  //alu Component
  val aluComponent = Module(new AluComponent)
  val aluCpOut     = aluComponent.access(srcs(0), srcs(1), aluType)
  asg(exeOut.wPrf.result, aluCpOut.res) //default,in mAlu may change it("AL")

  //bru
  if (main) {
    val brType       = uOp.brType.get
    val isBr         = brType =/= BranchType.NON
    val mispreBlkReg = RegInit(false.B)
    val brValid      = isBr && !mispreBlkReg && instValid
    //predict and gen
    val inBrInfo  = exeIn.branch.get
    val predict   = inBrInfo.predict
    val BrHandler = Module(new BrHandler)
    val genTaken =
      BrHandler.access(
        srcs(0),
        srcs(1),
        MuxCase(
          brType,
          Seq(
            (aluType === AluType.TRAPEQ) -> BranchType.BEQ,
            (aluType === AluType.TRAPNE) -> BranchType.BNE
          )
        )
      )
    val preCnt = predict.counter
    /*==================== Update BPU ====================*/
    val bpUp = bpuUpdate.get
    val btb  = bpUp.btb
    val pht  = bpUp.pht
    asg(bpUp.pc, inBrInfo.pcVal)
    // BTB update ====================================================
    asg(btb.bits.instType, inBrInfo.realBtbType)
    asg(btb.bits.target, inBrInfo.realTarget)
    asg(
      btb.valid,
      brValid && (inBrInfo.realBtbType =/= predict.btbType || inBrInfo.realTarget =/= predict.target)
    )
    // PHT update =====================================================
    val cat = Cat(preCnt, genTaken)
    asg(pht.valid, brValid && BranchType.isB(brType))
    asg(pht.bits, PatternHistoryTable.calNextCnt(preCnt, genTaken))
    // RAS update =====================================================
    val ras = Module(new RetAddrStack(false, retAddrStackSize))
    ras.io.push.valid := inBrInfo.realBtbType === BtbType.jcall && brValid
    ras.io.pop        := inBrInfo.realBtbType === BtbType.jret && brValid
    asg(ras.io.push.bits, inBrInfo.pcVal + 8.U)
    /*==================== MisPre Signal to Dper/ROB ====================*/
    val misSignal  = mispre.get
    val takenWrong = genTaken ^ predict.taken
    val destWrong  = genTaken && inBrInfo.realTarget =/= predict.target
    asg(misSignal.happen, brValid && (takenWrong || destWrong))
    asg(misSignal.realTarget, Mux(genTaken, inBrInfo.realTarget, inBrInfo.pcVal + 8.U(32.W)))
    asg(misSignal.robIdx, exeIn.robIndex)
    when(brValid && (takenWrong || destWrong)) {
      asg(mispreBlkReg, true.B)
      asg(exeOut.wbRob.isMispredict, true.B)
    }
    when(io.flush) { asg(mispreBlkReg, false.B) }
    //special:jrhb
    val jrhbSignal = Wire(Valid(UWord))
    asg(jrhbSignal.valid, brValid && brType === BranchType.JRHB)
    asg(jrhbSignal.bits, inBrInfo.realTarget)
    BoringUtils.addSource(jrhbSignal, "hbdest")
    //JRHB无论是否mispre都要设为mispre，ROB进行处理
    //前端无所谓是否重定向，由ROB重定向到JRHB的TARGET
    when(jrhbSignal.valid) { asg(exeOut.wbRob.isMispredict, true.B) }
    if (verilator && debug) {
      val backBrDiff = Module(new DifftestBackPred)
      asg(backBrDiff.io.clock, clock)
      asg(backBrDiff.io.en, io.out.fire && brValid)
      asg(backBrDiff.io.debugPC, io.out.bits.wbRob.debugPC.get)
      asg(backBrDiff.io.predDest, predict.target)
      asg(backBrDiff.io.predTake, predict.taken)
      asg(backBrDiff.io.realDest, inBrInfo.realTarget)
      asg(backBrDiff.io.predTake, predict.taken)
      asg(backBrDiff.io.realTake, genTaken)
      asg(backBrDiff.io.btbType, bpuUpdate.get.btb.bits.instType.asUInt)
    }
    //movz movn
    // val prevPDest = exeIn.prevPDest
    // val prevData  = Wire(UWord)
    // BoringUtils.addSource(prevPDest, "MOVZNPREVIDX")
    // BoringUtils.addSink(prevData, "MOVZNPREVDATA")
    val isMovzn = aluType === AluType.MOVN || aluType === AluType.MOVZ
    val wprf    = exeOut.wPrf
    val movWen =
      (aluType === AluType.MOVN && !BrHandler.io.rtEqZero) || (aluType === AluType.MOVZ && BrHandler.io.rtEqZero)
    when(isMovzn) {
      asg(wprf.result, Mux(movWen, srcs(0), exeIn.prevData))
    }

    when(aluType.isOneOf(AluType.TRAPEQ, AluType.TRAPNE)) {
      asg(outExDetect.happen, genTaken)
    }
  }

  /**
    * deal with exception
    *   priority:
    *   overflow < 保留指令例外/BP/SYS(DPER) < 取指TLB(IF1) < 中断
    */
  when(!inExDetect.happen && aluCpOut.overflow) {
    asg(outExDetect.happen, true.B)
    asg(outExDetect.excCode, ExcCode.Ov)
  }
}
