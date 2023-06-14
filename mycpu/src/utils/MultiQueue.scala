package utils
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.log2Up

/**
  * all the port should at least be fmt "1..0.."
  * all the gen in Q is "valid"
  *
  * push(x).valid can be different according to input,
  * but push(x).ready from MultiQueue must be same.
  * MultiQueue will accept them at same cycle when it has enough space(enqNum)
  *   or give x ready when it has x blank space(x<=enqNum)
  *
  * pop(x).valid also can be different according to
  * MultiQueue has enough data or not.
  * but pop(x).ready must be same,
  * only when (0 to deqNum).foldLeft(true.B)(pop(_).fire && _) === true.B
  * MultiQueue update it pop
  *   not need to be same,but should be "1...0..." form
  *
  * MultiQueue can be used in InstBuffer, FreeList(a little weird), ROB ,STQ
  *
  * @param enqNum enqueue number in one cycle
  * @param deqNum enqueue number in one cycle
  * @param gen data type
  * @param size Queue size
  */

//TODO:ROB need an out index
class MultiQueue[T <: Data](enqNum: Int, deqNum: Int, gen: T, size: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val push = Vec(enqNum, Flipped(Decoupled(gen)))
    val pop  = Vec(deqNum, Decoupled(gen))
  })

  //ring means "+"
  val ringBuffer     = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))
  val ringBufferHead = RegInit(0.U(log2Up(size).W))
  val ringBufferTail = RegInit(0.U(log2Up(size).W))

  //push
  val allowIn = (1 to enqNum).map(i => (ringBufferHead + i.U =/= ringBufferTail)).reduce(_ & _)
  (0 until enqNum).map(i => (io.push(i).ready := allowIn))
  val enqFireNum = (0 to enqNum).map(i => io.push(i).fire.asUInt).reduce(_ +& _)
  List.tabulate(enqNum)(i => {
    when(io.push(i).fire) {
      ringBuffer(ringBufferHead + i.U) := io.push(i).bits
    }
  })
  ringBufferHead := ringBufferHead + enqFireNum

  //pop
  (0 to enqNum).map(i => (io.pop(i).valid := (ringBufferTail + i.U =/= ringBufferHead)))
  val deqFireNum = (0 to deqNum).map(i => io.pop(i).fire.asUInt).reduce(_ +& _)
  ringBufferTail := ringBufferTail + deqFireNum
}
