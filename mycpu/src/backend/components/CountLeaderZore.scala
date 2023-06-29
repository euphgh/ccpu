package backend.components
import chisel3._
import config._
import chisel3.util._

class CountLeadZero extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val src  = UInt(32.W)
      val zero = Bool()
    }))
    val out = Valid(UWord)
  })
  val inBits = io.in.bits

  val widthCounter              = Counter(32)
  val sum                       = RegInit(0.U(32.W))
  val tmp                       = RegInit(0.U(32.W))
  val idle :: run :: res :: Nil = Enum(6)
  val state                     = RegInit(run)
  switch(state) {
    is(idle) {
      state := Mux(io.in.valid, run, idle)
      sum   := 0.U
      tmp   := inBits.src
      widthCounter.reset()
    }
    is(run) {
      sum   := sum + (tmp(31) === inBits.zero)
      state := Mux(widthCounter.inc(), res, run)
    }
    is(res) {
      state := idle
    }
  }
  io.out.valid := state === res
  io.out.bits  := sum
}
