package backend

import config._
import bundle._
import chisel3._
import chisel3.util._
import utils._
import backend.components._

// automat for status change when madd and msub
class Mdu extends FuncUnit(FuType.Mdu) {

  val robRetire = IO(Flipped(Valid(new SingleRetireBundle)))
  val c0Inst = IO(new Bundle {
    val mtc0 = (new Mtc0Bundle)
    val mfc0 = new Bundle {
      val addr  = Output(CP0Idx)
      val rdata = Input(UWord)
    }
  })

  //stage connect
  val exeStageIO = Wire(new ExeStageIO(FuType.Mdu))
  exeStageIO.out <> io.out
  PipelineConnect(roStage.io.out, exeStageIO.in, exeStageIO.out.fire, io.flush)
  val exeIn  = exeStageIO.in.bits
  val exeOut = exeStageIO.out.bits

  //unchange connect
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wbRob.isMispredict, false.B) //must set to false,and will not change it
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)
  asg(exeOut.wbRob.exDetect, exeIn.exDetect) //no exception happen here
  if (debug) asg(exeOut.wbRob.debugPC.get, exeIn.debugPC.get)

  import MduType._
  // alias  ==========================================================
  val (instValid, srcs, mduType) = (exeStageIO.in.valid, exeIn.srcData, exeIn.uOp.mduType.get)
  val isDiv                      = (mduType.isOneOf(DIV, DIVU)) && instValid
  val isMult                     = (mduType.isOneOf(MULT, MULTU, MUL, MADD, MADDU, MSUB, MSUBU)) && instValid
  val notMul                     = mduType =/= MduType.MUL
  val isClz                      = (mduType === CLZ) && instValid
  val isHi                       = (mduType.isOneOf(MFHI, MTHI)) && instValid
  val isLo                       = (mduType.isOneOf(MFLO, MTLO)) && instValid
  val isMtc0                     = (mduType === MduType.MTC0) && instValid
  val isMfc0                     = (mduType === MduType.MFC0) && instValid
  val isBlock                    = mduType.isOneOf(TLBP, MULT, MULTU, MUL, MADD, MADDU, MSUB, MSUBU, DIV, DIVU, CLZ) && instValid

  // multiplier ======================================================
  val mul   = Module(new MulComponent)
  val mulIn = mul.io.in.bits
  mulIn.isSign := mduType.isOneOf(Seq(MULT, MSUB, MADD, MUL))
  mulIn.isAdd  := mduType.isOneOf(MADD, MADDU)
  mulIn.isSub  := mduType.isOneOf(MSUB, MSUBU)
  mul.io.flush := io.flush
  (0 until srcDataNum).foreach(i => { mulIn.srcs(i) := srcs(i) })
  val multRes = mul.io.out.bits

  // divider =========================================================
  val div   = Module(new Divider)
  val divIn = div.io.in.bits
  divIn.isSign := mduType === DIV
  div.io.flush := io.flush
  (0 until srcDataNum).foreach(i => { divIn.srcs(i) := srcs(i) })
  val divRes = div.io.out.bits

  // count leader ====================================================
  val clz = Module(new CountLeadZero)
  clz.io.in.bits.src  := srcs(0)
  clz.io.in.bits.zero := true.B
  clz.io.flush        := io.flush

  //mfc0 =============================================================
  val c0Addr = srcs(1)(7, 0)
  asg(c0Inst.mfc0.addr, c0Addr)
  val c0Rdata = c0Inst.mfc0.rdata

  // tlb instr ========================================================
  import chisel3.util.experimental.BoringUtils._
  // only tlbp will block, need res
  val tlbpReq  = Wire(Bool())
  val tlbpRes  = Wire(Bool())
  val tlbrReq  = Wire(Bool())
  val tlbwiReq = Wire(Bool())
  val tlbwrReq = Wire(Bool())
  addSource(tlbpReq, "tlbpReq")
  addSink(tlbpRes, "tlbpRes")
  addSource(tlbrReq, "tlbrReq")
  addSource(tlbwiReq, "tlbwiReq")
  addSource(tlbwrReq, "tlbwrReq")
  if (verilator) addSource(exeIn.debugPC.get, "mduPC")

  val fuOutValid = List(mul.io.out.valid, div.io.out.valid, clz.io.out.valid, tlbpRes)
  //List(mul.io.out.valid, div.io.out.valid, clz.io.out.valid, tlbpRes)
  val fuOutData = List(multRes, divRes, clz.io.out.bits, 0.U)

  // speculate ============================================================================
  import config.MycpuInit.{HIReset, LOReset}
  val specHi = RegInit(UWord, HIReset)
  val specLo = RegInit(UWord, LOReset)
  addSource(specHi, "specHIdata")
  addSource(specLo, "specLOdata")
  val data64Q   = Module(new Queue(gen = UInt(64.W), entries = 4, hasFlush = true)) //muldiv
  val data32Q   = Module(new Queue(gen = UWord, entries = 4, hasFlush = true)) //mtc0 mthi mtlo
  val mtc0AddrQ = Module(new Queue(gen = CP0Idx, entries = 4, hasFlush = true)) //mtc0 addr
  asg(data64Q.io.flush.get, io.flush)
  asg(data32Q.io.flush.get, io.flush)
  asg(mtc0AddrQ.io.flush.get, io.flush)

  // automat ================================================================================
  val run :: block :: Nil = Enum(2)
  val blockDone           = RegInit(false.B)
  // state
  val state    = RegInit(run)
  val blockRes = WireInit(0.U(64.W))
  exeStageIO.out.valid := false.B //default
  switch(state) {
    is(run) {
      exeStageIO.out.valid := !isBlock && instValid
      state                := Mux(isBlock, block, run)
      blockDone            := false.B
    }
    is(block) {
      val validMask = VecInit(fuOutValid).asUInt
      when(validMask.orR) { blockDone := true.B }
      assert(PopCount(validMask) < 2.U) // one hot must
      // want to faster
      blockRes             := HoldUnless(Mux1H(validMask, fuOutData), validMask.orR)
      exeStageIO.out.valid := validMask.orR || blockDone
    }
  }
  when(io.flush || io.out.fire) {
    state     := run
    blockDone := false.B
  }

// all valid and ready ==================================================
  mul.io.in.valid := isMult && !blockDone
  div.io.in.valid := isDiv && !blockDone
  clz.io.in.valid := isClz && !blockDone
  tlbpReq         := instValid && mduType === TLBP && !blockDone
  val queueReadyIn = data32Q.io.enq.ready && data64Q.io.enq.ready && mtc0AddrQ.io.enq.ready
  exeStageIO.in.ready := queueReadyIn && exeStageIO.out.fire || !instValid // in.ready depand on out.valid

  // Output ===========================================================================
  exeOut.wPrf.result := MuxCase(
    blockRes, // include clz and mul, they are blocked and save result in it[低32位]
    Seq(
      isHi   -> specHi,
      isLo   -> specLo,
      isMfc0 -> c0Rdata
    )
  )
  exeOut.wPrf.wmask := Mux(mduType.isOneOf(MFC0, MFHI, MFLO, CLZ, MUL), "b1111".U, "b0000".U)

  /**
    * speculative:<exeStage>
    *   muldiv:write spec,data64 enq
    *   mthi mtlo:write spec,data32 enq
    *   mtc0:data32 enq,mtc0addr enq
    */
  asg(
    data64Q.io.enq.valid,
    ((isMult && notMul) || isDiv) && exeStageIO.out.fire
  ) //mul no need write hilo
  asg(data32Q.io.enq.valid, (mduType.isOneOf(MTHI, MTLO) || isMtc0) && exeStageIO.out.fire)
  asg(mtc0AddrQ.io.enq.valid, isMtc0 && exeStageIO.out.fire)
  tlbwiReq := instValid && mduType === TLBWI && exeStageIO.out.fire
  tlbwrReq := instValid && mduType === TLBWR && exeStageIO.out.fire
  tlbrReq  := instValid && mduType === TLBR && exeStageIO.out.fire

  asg(data64Q.io.enq.bits, blockRes)
  asg(data32Q.io.enq.bits, srcs(0)) //mtc0:rt mthilo:rs
  asg(mtc0AddrQ.io.enq.bits, c0Addr)

  val wdata64 = data64Q.io.enq.bits
  val wdata32 = data32Q.io.enq.bits
  when(exeStageIO.out.fire) {
    when(isDiv || (isMult && notMul)) { //mul no need write hilo
      asg(specHi, wdata64(63, 32))
      asg(specLo, wdata64(31, 0))
    }
    when(mduType === MTHI) { asg(specHi, wdata32) }
    when(mduType === MTLO) { asg(specLo, wdata32) }
  }

  /**
    * Arch:<retire stage>
    *   muldiv:write arch,data64 deq
    *   mthi mtlo:write arch,data32 enq
    *   mtc0:data32 deq,mtc0addr deq,give cp0 writeBundle
    */
  val archHi       = RegInit(UWord, HIReset)
  val archLo       = RegInit(UWord, LOReset)
  val commitData64 = data64Q.io.deq.bits
  val commitData32 = data32Q.io.deq.bits
  val retireVReg   = RegNext(robRetire.valid)
  val commit       = RegNext(robRetire.bits)
  when(retireVReg) {
    when(commit.muldiv) {
      asg(archHi, commitData64(63, 32))
      asg(archLo, commitData64(31, 0))
    }
    when(commit.mthi) { asg(archHi, commitData32) }
    when(commit.mtlo) { asg(archLo, commitData32) }
  }
  asg(c0Inst.mtc0.wen, retireVReg && commit.mtc0)
  asg(c0Inst.mtc0.wdata, commitData32)
  asg(c0Inst.mtc0.waddr, mtc0AddrQ.io.deq.bits)

  asg(data32Q.io.deq.ready, retireVReg && (commit.mthi || commit.mtlo || commit.mtc0))
  asg(data64Q.io.deq.ready, retireVReg && commit.muldiv)
  asg(mtc0AddrQ.io.deq.ready, retireVReg && commit.mtc0)

  /**
    * recover specHiLo when flush
    */
  val flushReg = RegNext(io.flush)
  when(flushReg) {
    asg(specHi, archHi)
    asg(specLo, archLo)
  }

  // DiffTest ============================================================
  val checkHiLoEn = RegNext(retireVReg && (commit.muldiv || commit.mthi || commit.mtlo))
  import difftest.DifftestArchHILO
  if (verilator) {
    val difftestHILO = Module(new DifftestArchHILO)
    difftestHILO.io.hi := archHi
    difftestHILO.io.lo := archLo
    asg(difftestHILO.io.en, checkHiLoEn)
    difftestHILO.io.clock := clock
  }
}
