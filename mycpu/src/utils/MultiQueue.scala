package utils
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled

/**
  * push(x).valid can be different according to input,
  * but push(x).ready from MultiQueue must be same.
  * MultiQueue will accept them at same cycle when it has enough space
  *
  * pop(x).valid also can be different according to
  * MultiQueue has enough data or not.
  * but pop(x).ready must be same,
  * only when (0 to deqNum).foldLeft(true.B)(pop(_).fire && _) === true.B
  * MultiQueue update it pop
  *
  * MultiQueue can be used in InstBuffer, FreeList, ROB
  *
  * @param enqNum enqueue number in one cycle
  * @param deqNum enqueue number in one cycle
  * @param gen data type
  */
class MultiQueue[T <: Data](enqNum: Int, deqNum: Int, gen: T) extends MycpuModule {
  val io = IO(new Bundle {
    val push = Vec(enqNum, Flipped(Decoupled(gen)))
    val pop  = Vec(deqNum, Decoupled(gen))
  })
}
