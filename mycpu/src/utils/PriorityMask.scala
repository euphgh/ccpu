package utils
import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.decoder
import chisel3.util.experimental.decode.TruthTable

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
