package utils

import chisel3._
import chisel3.util._
import scala.util.Random

object PatRand {
  def apply(pat: BitPat): UInt = {
    val bitString = pat.toString().substring(7).init.reverse
    val uintVal = bitString.foldRight(BigInt(0)) {
      case (c, acc) => {
        (acc << 1) + (if (c == '?') BigInt(Random.nextInt(2)) else BigInt(c.asDigit))
      }
    }
    uintVal.U(pat.getWidth.W)
  }
}
