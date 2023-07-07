package utils
import chisel3._
import chisel3.util.log2Ceil
import chisel3.util.Cat

object StoreQUtils {
  def getByteIndex(enqPtr: UInt, deqPtr: UInt, matchWen: UInt, entries: Int) = {
    val counterWidth = log2Ceil(entries)
    val ptrWidth     = counterWidth + 1

    require(enqPtr.getWidth == ptrWidth)
    require(deqPtr.getWidth == ptrWidth)
    require(matchWen.getWidth == entries)

    val res = WireInit(0.U(counterWidth.W)) //init
    (0 until entries).map(i => {
      val idx     = (deqPtr + i.U)(ptrWidth - 1, 0)
      val realIdx = idx(counterWidth - 1, 0)
      //val realIdx = Cat(0.U, idx(counterWidth - 1, 0))
      val valid = Mux(deqPtr < enqPtr, idx < enqPtr, idx(ptrWidth - 1) | realIdx < enqPtr)
      when(matchWen(realIdx) && valid) {
        asg(res, realIdx)
        //asg(res, realIdx(counterWidth - 1, 0))
      }
    })
    res
  }
}
