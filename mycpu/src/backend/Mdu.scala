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
}
class Divider extends MycpuModule {
  val io = IO(new MulDivIO)
  //TODO:deal with readyGo logic
  val readyGo = Wire(Bool())
  io.in.ready  := !io.in.valid || readyGo && io.out.ready
  io.out.valid := io.in.valid && readyGo
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
}

// automat for status change when madd and msub
class Mdu extends FuncUnit(FuType.Mdu) {

  val archHi = IO(Input(UWord))
  val archLo = IO(Input(UWord))

  //stage connect
  val exeStageIO = new ExeStageIO(FuType.Mdu)
  exeStageIO.out <> io.out
  PipelineConnect(roStage.io.out, exeStageIO.in, exeStageIO.out.fire, io.flush)
  val exeIn                      = exeStageIO.in.bits
  val exeOut                     = exeStageIO.out.bits
  val (instValid, srcs, mduType) = (exeStageIO.in.valid, exeIn.srcData, exeIn.decoded.memType)

  //unchange connect
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wbRob.memReqVaddr, 0.U(vaddrWidth.W)) //DontCare
  asg(exeOut.wbRob.isMispredict, false.B) //DontCare
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)
  asg(exeOut.wbRob.exception, exeIn.exception) //no exception happen here

  //this can be write in an object , use def
  //fuSel shoule be one-hot
  val isSign = (mduType === MduType.DIV || mduType === MduType.MULT)
  val isDiv  = (mduType === MduType.DIV || mduType === MduType.DIVU) && instValid
  val isMult = (mduType === MduType.MULT || mduType === MduType.MULTU) && instValid
  val isClz  = (mduType === MduType.CLZ) && instValid
  val isHi   = (mduType === MduType.MFHI || mduType === MduType.MTHI) && instValid
  val isLo   = (mduType === MduType.MFLO || mduType === MduType.MTLO) && instValid

  //note that the hilo here is speculative
  //arch-hilo can write in when flush
  val hiReg = RegInit(UWord, 0.U)
  val loReg = RegInit(UWord, 0.U)
  hiReg := RegEnable(srcs(0), instValid && (mduType === MduType.MTHI))
  loReg := RegEnable(srcs(0), instValid && (mduType === MduType.MTLO))
  when(io.flush) {
    hiReg := archHi
    loReg := archLo
  }

  //3 fu here
  val mul = Module(new Multiplier)
  val div = Module(new Divider)
  val clz = Module(new CountLeadZeor)

  //feed data
  List(mul.io, div.io).map {
    case x =>
      asg(x.in.bits.isSign, isSign)
      asg(x.in.bits.srcs, srcs)
  }
  asg(clz.io.in.bits, srcs(0))

  //deal with fu.in.valid and fu.out.ready
  val fuSel = VecInit(isMult, isDiv, isClz, isHi, isLo, !instValid)
  val fuIn  = VecInit(mul.io.in, div.io.in, clz.io.in)
  val fuOut = VecInit(mul.io.out, div.io.out, clz.io.out)
  (0 to 2).map(i => {
    asg(fuIn(i).valid, fuSel(i))
    asg(fuOut(i).ready, exeStageIO.out.ready)
  })

  /**
    * deal with exeStge in.ready and out.valid
    *   common form is:
    *     out.valid = pipex_valid & readyGo
    *     in.ready  = !pipex_valid || readyGo & io.out.ready
    *   we have dealt with valid-rdy inside fu
    *     when certain fu has been selected(instvalid & kindMatch),use its out.valid and in.ready
    *   when hilo(instvalid & kindMatch):
    *     hilo inst always readyGo
    *     so out.valid is true,so in.ready = out.ready
    */
  asg(
    exeStageIO.out.valid,
    Mux1H(fuSel, VecInit(fuOut(0).valid, fuOut(1).valid, fuOut(2).valid, true.B, true.B, false.B))
  )
  asg(
    exeStageIO.in.ready,
    Mux1H(fuSel, VecInit(fuIn(0).ready, fuIn(1).ready, fuIn(2).ready, io.out.ready, io.out.ready, true.B))
  )

  //get the result
  asg(
    exeOut.wPrf.result,
    Mux1H(fuSel, VecInit(fuOut(0).bits, fuOut(1).bits, fuOut(2).bits, hiReg, loReg, DontCare))
  )

}
