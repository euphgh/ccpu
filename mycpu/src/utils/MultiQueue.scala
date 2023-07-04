package utils
import config._
import bundle._
import chisel3._
import chisel3.util._
import backend.RobEntry

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

class MultiQueue[T <: Data](
  enqNum: Int,
  deqNum: Int,
  gen:    T,
  size:   Int     = 32,
  allIn:  Boolean = false,
  isFL:   Boolean = false)
    extends MycpuModule {
  require(isPow2(size))
  val counterWidth = log2Ceil(size)
  val ptrWidth     = counterWidth + 1

  val io = IO(new Bundle {
    val push    = Vec(enqNum, Flipped(Decoupled(gen)))
    val pop     = Vec(deqNum, Decoupled(gen))
    val flush   = Input(Bool())
    val tailPtr = Output(UInt(ptrWidth.W))
    val headPtr = Output(UInt(ptrWidth.W))
  })

  //ring means "+"
  val initZero   = Seq.fill(size)(0.U.asTypeOf(gen))
  val initFl     = (0 until size).map(i => (i + 32).U.asTypeOf(gen))
  val ringBuffer = RegInit(VecInit(if (isFL) initFl else initZero))

  val headPtr    = RegInit(UInt(ptrWidth.W), if (isFL) size.U else 0.U)
  val tailPtr    = RegInit(UInt(ptrWidth.W), 0.U)
  val deqFireNum = PopCount(io.pop.map(_.fire))
  val enqFireNum = PopCount(io.push.map(_.fire))
  def overflow(add:  UInt) = (headPtr - tailPtr + add - deqFireNum)(counterWidth) //must look ahead a cycle
  def underflow(sub: UInt) = (headPtr - tailPtr - sub)(counterWidth) //only considerate current
  val nextBasicNum = RegNext(headPtr + enqFireNum - tailPtr - deqFireNum)
  def overflowR(add:  UInt) = (nextBasicNum + add - deqFireNum)(counterWidth) //must look ahead a cycle
  def underflowR(sub: UInt) = (nextBasicNum - sub)(counterWidth) //only considerate current
  val counterMatch = headPtr(counterWidth - 1, 0) === tailPtr(counterWidth - 1, 0)
  val signMatch    = headPtr(ptrWidth - 1) === tailPtr(ptrWidth - 1)
  val empty        = counterMatch && signMatch
  val full         = counterMatch && !signMatch
  io.headPtr := headPtr
  io.tailPtr := tailPtr

  //assume input is 1..0..
  if (allIn) {
    val enqValidNum = PopCount(io.push.map(_.valid))
    (0 until enqNum).foreach(i => io.push(i).ready := !(overflowR(enqValidNum - 1.U)))
  } else {
    (0 until enqNum).foreach(i => io.push(i).ready := !(overflowR(i.U)))
  }
  List.tabulate(enqNum)(i => {
    when(io.push(i).fire) {
      ringBuffer(headPtr + i.U) := io.push(i).bits
    }
  })
  headPtr := headPtr + enqFireNum

  //pop
  (0 until deqNum).foreach(i => (io.pop(i).valid := !underflowR((i + 1).U)))
  tailPtr := tailPtr + deqFireNum
  (0 until deqNum).foreach(i => {
    io.pop(i).bits := ringBuffer(tailPtr + i.U)
  })

  when(io.flush) {
    headPtr := 0.U
    tailPtr := 0.U
  }
}
