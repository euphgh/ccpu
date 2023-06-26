package utils
import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.decoder
import chisel3.util.experimental.decode.TruthTable
import chisel3.util.PriorityEncoderOH
import chisel3.util.experimental.decode.QMCMinimizer

object PriorityMask {
  def apply(input: UInt): UInt = {
    val n = input.getWidth
    val pair = (0 until n).map(i => {
      val left  = "b" + "?" * (n - i - 1) + "1" + "0" * i
      val right = "b" + "1" * (n - i) + "0"
      BitPat(left) -> BitPat(right)
    })
    decoder(QMCMinimizer, input, TruthTable(pair, BitPat("b" + "1" * n)))
  }
}

/**
  * 计算从低位到高位连续1的个数
  * 输入必须保证如果有1只能从0位开始，且必须连续
  */
object PriorityCount {
  def apply(input: UInt): UInt = {
    val n = input.getWidth
    val pair = (0 until n).map(i => {
      val left  = "b" + "?" * (n - i - 1) + "1" + "0" * i
      val right = i.U
      BitPat(left) -> BitPat(right)
    })
    decoder(QMCMinimizer, input, TruthTable(pair, BitPat("b" + "1" * n)))
  }
  def apply(input: Vec[Bool]): UInt = {
    apply(input.asUInt)
  }
}
object SecondPriEncoder {
  def apply(input: UInt): UInt = {
    val pri = PriorityEncoderOH(input)
    PriorityEncoderOH(pri & input)
  }
}

object CountMask {

  /** Count input 1 number and generate a UInt has consecutive 1 from bit 0
    * @example {{{
    * CountMask("b000") = b000
    * CountMask("b001") = b001
    * CountMask("b101") = b011
    * CountMask("b110") = b011
    * CountMask("b111") = b111
    * }}}
    */
  def apply(input: UInt): UInt = {
    val maxWidth      = input.getWidth
    val binaryStrings = (0 to maxWidth).map(Integer.toBinaryString(_))
    val bitPats = binaryStrings
      .map(str => {
        val onesCount = str.count(_ == '1')
        (
          BitPat("b" + str.reverse.padTo(maxWidth, '0').reverse),
          BitPat("b" + "0" * (maxWidth - onesCount) + "1" * (onesCount))
        )
      })
      .toList
    decoder(QMCMinimizer, input, TruthTable(bitPats, BitPat("b" + "0" * maxWidth)))
  }

  /** generate oneHot version not Priority
    * @example {{{
    * CountMask("b000") = b000
    * CountMask("b001") = b001
    * CountMask("b101") = b010
    * CountMask("b110") = b010
    * CountMask("b111") = b100
    * }}}
    */
  def oneHot(input: UInt): UInt = {
    val maxWidth      = input.getWidth
    val binaryStrings = (0 to maxWidth).map(Integer.toBinaryString(_))
    val bitPats = binaryStrings
      .map(str => {
        val onesCount = str.count(_ == '1')
        (
          BitPat("b" + str.reverse.padTo(maxWidth, '0').reverse),
          BitPat("b" + "0" * (maxWidth - onesCount) + "1" + "0 " * (onesCount - 1))
        )
      })
      .toList
    decoder(QMCMinimizer, input, TruthTable(bitPats, BitPat("b" + "0" * maxWidth)))
  }
}
