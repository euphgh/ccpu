package utils
import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.decoder
import chisel3.util.experimental.decode.TruthTable
import chisel3.util.PriorityEncoder

object PriorityVec {
  def apply(inputs: Vec[UInt]): UInt = {
    val or    = inputs.foldRight(0.U)(_ | _)
    val inPri = inputs.map(PriorityEncoder(_))
    val orPri = PriorityEncoder(or)
    VecInit((0 until inputs.size).map(inPri(_) === orPri)).asUInt
  }
}

object PriorityMask {
  def apply(input: UInt): UInt = {
    val n = input.getWidth
    val pair = (0 until n).map(i => {
      val left  = "b" + "?" * (n - i - 1) + "1" + "0" * i
      val right = "b" + "1" * (n - i) + "0"
      BitPat(left) -> BitPat(right)
    })
    decoder(input, TruthTable(pair, BitPat("b" + 0 * n)))
  }
}
