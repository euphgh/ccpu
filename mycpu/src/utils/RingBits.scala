package utils
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._

object RingBits {
  def rotateRight[T](seq: Seq[T], n: Int): Seq[T] = {
    val size           = seq.size
    val rotations      = if (size > 0) (size - (n % size)) % size else 0
    val rotatedIndices = (rotations until size) ++ (0 until rotations)
    rotatedIndices.map(seq)
  }

  /**
    * @example {{{
    * when inner = 4 outer = 8
    * 000 -> 000, 001, 010, 011
    * 001 -> 100, 001, 010, 011
    * 010 -> 100, 101, 010, 011
    * 011 -> 100, 101, 110, 011
    * 100 -> 100, 101, 110, 111
    * 101 -> 000, 101, 110, 111
    * 110 -> 000, 001, 110, 111
    * 111 -> 000, 001, 010, 111
    *
    * when inner = 2 outer = 8
    * 000 -> 000, 001
    * 001 -> 010, 001
    * 010 -> 010, 011
    * 011 -> 100, 011
    * 100 -> 100, 101
    * 101 -> 110, 101
    * 110 -> 110, 111
    * 111 -> 000, 111
    * }}}
    * @param inner
    * @param outer
    * @param channel
    */
  def apply(input: UInt, inner: Int, channel: Int) = {
    val outer = math.pow(2, input.getWidth).toInt
    require(isPow2(inner))
    require(isPow2(outer))
    val ioWidth = log2Ceil(outer)
    require(outer > inner)
    require(channel < inner)
    require(channel >= 0)
    val bitPats = (0 until outer).map(i => {
      val orderSeq = (0 until inner).map(j => {
        BitPat(((i + j) % outer).U(ioWidth.W))
      })
      BitPat(i.U(ioWidth.W)) -> rotateRight(orderSeq, i % inner)
    })
    decoder(QMCMinimizer, input, TruthTable(bitPats.map(i => i._1 -> i._2(channel)), BitPat("b" + "?" * ioWidth)))
  }
}
