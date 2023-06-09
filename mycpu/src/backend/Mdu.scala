package backend

import bundle._
import config._
import chisel3._
import chisel3.util._

class MulDivIO extends MycpuBundle {
  val in = Flipped(Decoupled(new Bundle {
    val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
    val isSign = Output(Bool())
  }))
  val out = DecoupledIO(Output(UInt((dataWidth * 2).W)))
}

class Multiplier extends MycpuModule {
  val io = IO(new MulDivIO)
  //io.out.bits
  //io.out.valid := F(io.in.fire())
  val busy = RegInit(false.B)
  when(io.in.valid && !busy) { busy := true.B }
  when(io.out.valid) { busy := false.B }
  io.in.ready := !busy
}
class Divider extends MycpuModule {
  val io = IO(new MulDivIO)
  //automachine
}

class Mdu extends FuncUnit(FuType.Mdu) {
  val mul        = Module(new Multiplier)
  val div        = Module(new Divider)

  val hiReg = Mem(1, UWord)
  val loReg = Mem(1, UWord)
}
