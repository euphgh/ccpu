package config
import chisel3._

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
  val MULT, MULTU, DIV, DIVU, MFHI, MFLO, MTHI, MTLO, CLZ = Value
}
object SpecialType extends ChiselEnum {
  val LOAD, STORE, MTC0, MTHI, MTLO, ERET, NON = Value
}
object BlockType extends ChiselEnum {
  val CACHEINST, MFC0, NON = Value
}
