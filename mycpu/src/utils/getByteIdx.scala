package utils
import chisel3._
import chisel3.util.log2Ceil
import chisel3.util.Cat
import chisel3.util.isPow2
import chisel3.util.BitPat
import chisel3.util.UIntToOH

object StoreQUtils {
  def getByteIndex(enqPtr: UInt, deqPtr: UInt, matchWen: UInt, entries: Int) = {
    val counterWidth = log2Ceil(entries)
    val ptrWidth     = counterWidth + 1

    require(enqPtr.getWidth == ptrWidth)
    require(deqPtr.getWidth == ptrWidth)
    require(matchWen.getWidth == entries)

    val res      = WireInit(0.U(counterWidth.W)) //init
    val getValid = WireInit(false.B)
    (0 until entries).map(i => {
      val idx     = (deqPtr + i.U)(ptrWidth - 1, 0)
      val realIdx = idx(counterWidth - 1, 0)
      val valid = Mux(
        deqPtr === enqPtr,
        false.B,
        Mux(deqPtr < enqPtr, (idx < enqPtr) && (idx >= deqPtr), idx(ptrWidth - 1) | realIdx < enqPtr)
      )
      when(matchWen(realIdx) && valid) {
        asg(res, realIdx)
        asg(getValid, true.B)
      }
    })
    (res, getValid)
  }
  def getOHIndex(enqPtr: UInt, deqPtr: UInt, matchWen: UInt, entries: Int) = {
    val res = getByteIndex(enqPtr, deqPtr, matchWen, entries)
    Mux(res._2, UIntToOH(res._1), 0.U)
  }
  def getOHIndexDecode(enqPtr: UInt, deqPtr: UInt, matchWen: UInt, entries: Int) = {
    val ptrs = new PtrPriority(entries)
    ptrs.codeOH(deqPtr, enqPtr, matchWen)
  }
}

class PtrPriority(val entries: Int) {
  require(isPow2(entries))
  val ring       = entries << 1
  val countWidth = log2Ceil(entries)
  val ptrWidth   = countWidth + 1
  def strTupleOfDeq0(n: Int) = {
    require(n <= entries)
    if (n == 0) Seq("?" * entries -> "0" * entries)
    else {
      val leftQnum = entries - n
      val res = (1 to n).map(i => {
        val left  = "0" * (i - 1) + "1" + "?" * (n - i)
        val right = "0" * (i - 1) + "1" + "0" * (n - i)
        ("?" * leftQnum + left) -> ("0" * leftQnum + right)
      })
      res ++
        Seq(("?" * leftQnum + "0" * (entries - leftQnum)) -> ("0" * entries))
    }
  }
  private def leftRotateOnce(str: String): String = {
    if (str.isEmpty) str
    else str.tail + str.head
  }
  private def leftRotateN(str: String, n: Int): String = {
    var result = str
    for (_ <- 1 to n) {
      result = leftRotateOnce(result)
    }
    result
  }
  def oneGroup(deq: Int) = {
    (0 to entries).map(gap => {
      val enq          = (deq + gap) % ring
      val pairs        = strTupleOfDeq0(gap)
      val rotatedPairs = pairs.map(pair => (leftRotateN(pair._1, deq) -> leftRotateN(pair._2, deq)))
      (deq, enq, rotatedPairs)
    })
  }
  def allGroups = {
    (0 until ring).map(oneGroup(_))
  }
  def bitPats = {
    val foo = allGroups
    foo.flatMap(oneGroup => {
      oneGroup.flatMap(ptrPairs => {
        val deq = BitPat(ptrPairs._1.U(ptrWidth.W))
        val enq = BitPat(ptrPairs._2.U(ptrWidth.W))
        ptrPairs._3.map(pair => deq ## enq ## BitPat("b" + pair._1) -> BitPat("b" + pair._2))
      })
    })
  }
  def codeOH(deq: UInt, enq: UInt, valids: UInt): UInt = {
    require(deq.getWidth == ptrWidth)
    require(enq.getWidth == ptrWidth)
    require(valids.getWidth == entries)
    import chisel3.util.experimental.decode._
    val res = decoder(QMCMinimizer, Cat(deq, enq, valids), TruthTable(bitPats, BitPat("b" + "?" * entries)))
    require(res.getWidth == entries)
    res
  }
}

class StoreQPriority0(val entries: Int) extends Module {
  val ptrs    = new PtrPriority(entries)
  val deq     = IO(Input(UInt(ptrs.ptrWidth.W)))
  val enq     = IO(Input(UInt(ptrs.ptrWidth.W)))
  val valids  = IO(Input(UInt(ptrs.entries.W)))
  val oneHots = IO(Output(UInt(ptrs.entries.W)))
  oneHots := ptrs.codeOH(deq, enq, valids)
}
class StoreQPriority1(val entries: Int) extends Module {
  require(isPow2(entries))
  val ring       = entries << 1
  val countWidth = log2Ceil(entries)
  val ptrWidth   = countWidth + 1
  val deq        = IO(Input(UInt(ptrWidth.W)))
  val enq        = IO(Input(UInt(ptrWidth.W)))
  val valids     = IO(Input(UInt(entries.W)))
  val oneHots    = IO(Output(UInt(entries.W)))
  oneHots := StoreQUtils.getByteIndex(enq, deq, valids, entries)._1
}

object SQMain extends App {
  val foo = new PtrPriority(4)
  println(foo.bitPats.size)
  println(foo.allGroups.mkString("\n"))
}
