package utils

import java.math.BigInteger

object UnsignUtils {
  def toUsDec(bi: BigInt): String = {
    val bytes         = bi.toByteArray
    val unsignedBytes = if (bytes(0) == 0) bytes.tail else bytes
    val bigInteger    = new BigInteger(1, unsignedBytes)
    bigInteger.toString(10)
  }
  def toUsHex(bi: BigInt): String = {
    val bytes         = bi.toByteArray
    val unsignedBytes = if (bytes(0) == 0) bytes.tail else bytes
    val bigInteger    = new BigInteger(1, unsignedBytes)
    "0x" + bigInteger.toString(16)
  }
}
