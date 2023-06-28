package config
import chisel3._
import chisel3.util._

object SRCType extends ChiselEnum {
  val RSRT, RS, RT, noSRC = Value
}

object DSTType extends ChiselEnum {
  val toRD, to31, toRT, noDST = Value
}

object ChiselFuType extends ChiselEnum {
  val MainALU, SubALU, LSU, MDU = Value
}

object MduType extends ChiselEnum {
  val MULT, MULTU, DIV, DIVU, MFHI, MFLO, MTHI, MTLO, CLZ, MTC0, MFC0 = Value
}
object SpecialType extends ChiselEnum {
  val LOAD, STORE, MTC0, MTHI, MTLO, MULDIV, ERET, CACHEINST, HB, NON = Value
}
object BlockType extends ChiselEnum {
  val CACHEINST, MFC0, NON = Value
}

object MemType extends ChiselEnum {
  // group word
  val LW = Value("b0000".U)
  val SW = Value("b0001".U)
  // group left
  val LWL = Value("b0010".U)
  val SWL = Value("b0011".U)
  // group right
  val LWR = Value("b0100".U)
  val SWR = Value("b0101".U)
  // group byte0
  val LB  = Value("b1000".U)
  val LBU = Value("b1001".U)
  val SB  = Value("b1010".U)
  // group half
  val LH  = Value("b1100".U)
  val LHU = Value("b1101".U)
  val SH  = Value("b1110".U)

  def wordPat  = BitPat("b000?")
  def leftPat  = BitPat("b001?")
  def rightPat = BitPat("b010?")
  def bytePat  = BitPat("b10??")
  def halfPat  = BitPat("b11??")
}
