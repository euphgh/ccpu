package utils
import chisel3._
import chisel3.util.Valid

class Mark[T <: Data](gen: T, init: T = 0.U) extends Module {
  val start = IO(Flipped(Valid(gen)))
  val end   = IO(Input(Bool()))
  val value = IO(Valid(gen))
  val isSet = IO(Output(Bool()))

  val set  = RegInit(false.B)
  val mark = RegInit(gen, init)
  when(start.valid && !set) {
    set  := true.B
    mark := start.bits
  }
  when(end) {
    set := false.B
  }
  asg(value.bits, mark)
  asg(value.valid, set)
  asg(isSet, set)
}
