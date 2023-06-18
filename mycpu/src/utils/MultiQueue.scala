package utils
import config._
import chisel3._
import chisel3.util._

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
  * @param allIn True:   if MultiQueue dont't have space accepts all enq elements, it will not assert any enq.ready
  *              false:  it will assert some enq.ready
  */

//TODO:ROB need an out index
class MultiQueue[T <: Data](enqNum: Int, deqNum: Int, gen: T, size: Int = 32, allIn: Boolean = false)
    extends MycpuModule {
  val io = IO(new Bundle {
    val push = Vec(enqNum, Flipped(Decoupled(gen)))
    val pop  = Vec(deqNum, Decoupled(gen))
  })
  require(isPow2(size))

  private val counterWidth = log2Up(size)
  private val ptrWidth     = counterWidth + 1

  //ring means "+"
  val ringBuffer         = RegInit(VecInit(Seq.fill(size)(0.U.asTypeOf(gen))))
  val headPtr            = RegInit(0.U, UInt(ptrWidth.W))
  private val tailPtr    = RegInit(0.U, UInt(ptrWidth.W))
  private val deqFireNum = PopCount(io.pop.map(_.fire))
  private def overflow(add:  UInt) = (headPtr - tailPtr + add - deqFireNum)(counterWidth + 1) //must look ahead a cycle
  private def underflow(sub: UInt) = (headPtr - tailPtr - sub)(counterWidth + 1) //only considerate current
  private val counterMatch = headPtr(counterWidth - 1, 0) === tailPtr(counterWidth - 1, 0)
  private val signMatch    = headPtr(ptrWidth - 1) === tailPtr(ptrWidth - 1)
  val empty                = counterMatch && signMatch
  val full                 = counterMatch && !signMatch

  //assume input is 1..0..
  if (allIn) {
    val enqValidNum = PopCount(io.push.map(_.valid))
    (0 to enqNum).foreach(i => io.push(i).ready := !(overflow(enqValidNum)))
  } else {
    (0 to enqNum).foreach(i => io.push(i).ready := !(overflow(i.U)))
  }
  val enqFireNum = PopCount(io.push.map(_.fire))
  List.tabulate(enqNum)(i => {
    when(io.push(i).fire) {
      ringBuffer(headPtr + i.U) := io.push(i).bits
    }
  })
  headPtr := headPtr + enqFireNum

  //pop
  (0 to enqNum).foreach(i => (io.pop(i).valid := !underflow(i.U)))
  tailPtr := tailPtr + deqFireNum
  List.tabulate(deqNum)(i => {
    when(io.pop(i).fire) {
      io.pop(i) := ringBuffer(tailPtr + i.U)
    }
  })
}
