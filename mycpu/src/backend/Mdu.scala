package backend

import config._
import bundle._
import chisel3._
import chisel3.util._
import utils._

class MulDivIO extends MycpuBundle {
  val in = Flipped(Decoupled(new Bundle {
    val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
    val isSign = Output(Bool())
  }))
  val out = DecoupledIO(Output(UInt((dataWidth * 2).W)))
}

class Multiplier extends MycpuModule {
  val io = IO(new MulDivIO)
  //TODO:deal with readyGo logic
  val readyGo = Wire(Bool())
  io.in.ready  := !io.in.valid || readyGo && io.out.ready
  io.out.valid := io.in.valid && readyGo

  // asg(readyGo, true.B)
  // asg(io.out.bits, 0.U(64.W))
}
class Divider extends MycpuModule {
  val io = IO(new MulDivIO)
  //TODO:deal with readyGo logic
  val readyGo = Wire(Bool())
  io.in.ready  := !io.in.valid || readyGo && io.out.ready
  io.out.valid := io.in.valid && readyGo

  // asg(readyGo, true.B)
  // asg(io.out.bits, 0.U(64.W))
}
class CountLeadZeor extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(UWord))
    val out = Decoupled(UWord)
  })
  //TODO:deal with readyGo logic
  val readyGo = Wire(Bool())
  io.in.ready  := !io.in.valid || readyGo && io.out.ready
  io.out.valid := io.in.valid && readyGo

  // asg(readyGo, true.B)
  // asg(io.out.bits, 0.U(32.W))
}

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


   val (instValid, srcs, mduType) = (exeStageIO.in.valid, exeIn.srcData, exeIn.decoded.mduType)
  val c0Addr                     = srcs(0)(7, 0)


  //unchange connect
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wbRob.isMispredict, false.B) //must set to false,and will not change it
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)
  asg(exeOut.wbRob.exception, exeIn.exception) //no exception happen here

  //6 "fu" here
  val mul     = Module(new Multiplier)
  val div     = Module(new Divider)
  val clz     = Module(new CountLeadZeor)
  val specHi  = RegInit(UWord, 0.U)
  val specLo  = RegInit(UWord, 0.U)
  val c0Rdata = c0Inst.mfc0.rdata
  val divRes  = div.io.out.bits
  val multRes = mul.io.out.bits

  //fuSel shoule be one-hot,notice instValid!!
  val isDiv  = (mduType === MduType.DIV || mduType === MduType.DIVU) && instValid
  val isMult = (mduType === MduType.MULT || mduType === MduType.MULTU) && instValid
  val isClz  = (mduType === MduType.CLZ) && instValid
  val isHi   = (mduType === MduType.MFHI || mduType === MduType.MTHI) && instValid
  val isLo   = (mduType === MduType.MFLO || mduType === MduType.MTLO) && instValid
  val isMtc0 = (mduType === MduType.MTC0) && instValid
  val isMfc0 = (mduType === MduType.MFC0) && instValid

  //TODO:dataQ size
  val data64Q   = Module(new Queue(gen = UInt(64.W), entries = 4, hasFlush = true)) //muldiv
  val data32Q   = Module(new Queue(gen = UWord, entries = 8, hasFlush = true)) //mtc0 mthi mtlo
  val mtc0AddrQ = Module(new Queue(gen = CP0Idx, entries = 4, hasFlush = true)) //mtc0 addr
  asg(data64Q.io.flush.get, io.flush)
  asg(data32Q.io.flush.get, io.flush)
  asg(mtc0AddrQ.io.flush.get, io.flush)

  /**
    * speculative:<exeStage>
    *   muldiv:write spec,data64 enq
    *   mthi mtlo:write spec,data32 enq
    *   mtc0:data32 enq,mtc0addr enq
    */
  asg(data64Q.io.enq.valid, (isMult || isDiv) && exeStageIO.out.fire)
  asg(data32Q.io.enq.valid, (mduType === MduType.MTHI || mduType === MduType.MTLO || isMtc0) && exeStageIO.out.fire)
  asg(mtc0AddrQ.io.enq.valid, isMtc0 && exeStageIO.out.fire)

  asg(data64Q.io.enq.bits, Mux(isDiv, divRes, multRes))
  asg(data32Q.io.enq.bits, Mux(isMtc0, srcs(1), srcs(0))) //mtc0:rt mthilo:rs
  asg(mtc0AddrQ.io.enq.bits, c0Addr)

  val wdata64 = data64Q.io.enq.bits
  val wdata32 = data32Q.io.enq.bits
  when(exeStageIO.out.fire) {
    when(isDiv || isMult) {
      asg(specHi, wdata64(63, 32))
      asg(specLo, wdata64(31, 0))
    }
    when(isHi) { asg(specHi, wdata32) }
    when(isLo) { asg(specLo, wdata32) }
  }

  /**
    * Arch:<retire stage>
    *   muldiv:write arch,data64 deq
    *   mthi mtlo:write arch,data32 enq
    *   mtc0:data32 deq,mtc0addr deq,give cp0 writeBundle
    */
  val archHi       = RegInit(UWord, 0.U)
  val archLo       = RegInit(UWord, 0.U)
  val commitData64 = data64Q.io.deq.bits
  val commitData32 = data32Q.io.deq.bits
  val commit       = robRetire.bits
  when(robRetire.valid) {
    when(commit.muldiv) {
      asg(archHi, commitData64(63, 32))
      asg(archLo, commitData64(31, 0))
    }
    when(commit.mthi) { asg(archHi, commitData32) }
    when(commit.mtlo) { asg(archLo, commitData32) }
  }
  asg(c0Inst.mtc0.wen, robRetire.valid && commit.mtc0)
  asg(c0Inst.mtc0.wdata, commitData32)
  asg(c0Inst.mtc0.waddr, mtc0AddrQ.io.deq.bits)

  asg(data32Q.io.deq.ready, robRetire.valid && (commit.mthi || commit.mtlo || commit.mtc0))
  asg(data64Q.io.deq.ready, robRetire.valid && commit.muldiv)
  asg(mtc0AddrQ.io.deq.ready, robRetire.valid && commit.mtc0)

  /**
    * recover specHiLo when flush
    */
  when(io.flush) {
    asg(specHi, archHi)
    asg(specLo, archLo)
  }

  // feed data/addr
  val mdIOlist = List(mul.io, div.io)
  val isSign   = (mduType === MduType.DIV || mduType === MduType.MULT)
  List.tabulate(2)(i => {
    asg(mdIOlist(i).in.bits.isSign, isSign)
    asg(mdIOlist(i).in.bits.srcs, srcs)
  })
  asg(clz.io.in.bits, srcs(0))
  asg(c0Inst.mfc0.addr, c0Addr)

  //deal with fu.in.valid and fu.out.ready
  val isMtMf = isHi | isLo | isMfc0 | isMtc0 //1 cycle inst
  val fuSel  = VecInit(isMult, isDiv, isClz, isMtMf, !instValid)
  val fuIn   = List(mul.io.in, div.io.in, clz.io.in)
  val fuOut  = List(mul.io.out, div.io.out, clz.io.out)
  (0 to 2).map(i => {
    asg(fuIn(i).valid, fuSel(i))
    asg(fuOut(i).ready, exeStageIO.out.ready)
  })

  /**
    * deal with exeStge in.ready and out.valid
    *   common form is:
    *     out.valid = pipex_valid & readyGo
    *     in.ready  = !pipex_valid || readyGo & io.out.ready
    *   we have dealt with valid-rdy inside fu(mul/div/clz)
    *     when certain fu has been selected(instvalid & kindMatch),use its out.valid and in.ready
    *     when ishi|islo|ismtc0|ismfc0:
    *       always readyGo
    *       so out.valid is true,so in.ready = out.ready
    *     when !instValid:
    *       out.valid=false,in.ready=true
    */
  asg(
    exeStageIO.out.valid,
    Mux1H(fuSel, VecInit(fuOut(0).valid, fuOut(1).valid, fuOut(2).valid, true.B, false.B))
  )

  //it's the most simple way to block exestage.in
  val queueReadyIn = data32Q.io.enq.ready && data64Q.io.enq.ready && mtc0AddrQ.io.enq.ready
  asg(
    exeStageIO.in.ready,
    Mux(
      !queueReadyIn,
      false.B,
      Mux1H(fuSel, VecInit(fuIn(0).ready, fuIn(1).ready, fuIn(2).ready, io.out.ready, true.B))
    )
  )

  //get the result
  val rdata = Mux(isHi, specHi, Mux(isLo, specLo, c0Rdata))
  asg(
    exeOut.wPrf.result,
    Mux(isClz, clz.io.out.bits, rdata)
  )
  asg(exeOut.wPrf.wmask, 15.U(4.W))

}
