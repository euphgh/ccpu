package utils
import chisel3._
object BytesWordUtils {
  def word2Bytes(word: UInt) = {
    require(word.getWidth == 32)
    val res = Wire(Vec(4, UInt(8.W)))
    (0 to 3).foreach(i => {
      res(i) := word((i + 1) * 8 - 1, i * 8)
    })
    res
  }
  def maskWord(bytes: Vec[UInt], mask: UInt): Vec[UInt] = {
    require(bytes.asUInt.getWidth == 32)
    require(mask.getWidth == 4)
    val res = Wire(Vec(4, UInt(8.W)))
    (0 to 3).foreach(i => {
      res(i) := Mux(mask(i), bytes(i), "h00".U)
    })
    res
  }
  def maskWord(word: UInt, mask: UInt): Vec[UInt] = {
    maskWord(word2Bytes(word), mask)
  }
  def mergeWords(oldWord: UInt, newWord: UInt, oldMask: UInt): UInt = {
    maskWord(word2Bytes(oldWord), oldMask).asUInt |
      maskWord(word2Bytes(newWord), ~oldMask).asUInt
  }
}
