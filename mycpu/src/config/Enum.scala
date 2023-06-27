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
  def isC0Inst(op: MduType.Type): Bool = op === MTC0 || op === MFC0
}
object SpecialType extends ChiselEnum {
  val LOAD, STORE, MTC0, MTHI, MTLO, MULDIV, ERET, NON = Value
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
  val LB  = Value("b0100".U)
  val LBU = Value("b0101".U)
  val SB  = Value("b0110".U)
  // group half
  val LH  = Value("b1000".U)
  val LHU = Value("b1001".U)
  val SH  = Value("b1010".U)

  def wordPat  = BitPat("b000?")
  def leftPat  = BitPat("b001?")
  def rightPat = BitPat("b010?")
  def bytePat  = BitPat("b01??")
  def halfPat  = BitPat("b10??")
}
object BranchType extends ChiselEnum {
  val BEQ, BNE, BGEZ, BLEZ, BLTZ, BGTZ, BLTZAL, BGEZAL, J, JAL, JR, JALR, JRHB, NON = Value
  def isB(op: BranchType.Type): Bool =
    op === BEQ || op === BNE || op === BGEZ || op === BLEZ || op === BLTZ || op === BGTZ || op === BLTZAL || op === BGEZAL
  def isAL(op: BranchType.Type): Bool =
    op === BLTZAL || op === BGEZAL || op === JAL || op === JALR
  def isJr(op: BranchType.Type): Bool = op === JR || op === JALR
  def isJ(op:  BranchType.Type): Bool = op === J || op === JAL
}
object AluType extends ChiselEnum {
  val ADD, SUB, AND, OR, NOR, XOR, SLT, SLTU, SLL, SRL, SRA, SLLV, SRLV, SRAV, LUI = Value
  def isAdd(op:  AluType.Type): Bool = op === ADD
  def isSub(op:  AluType.Type): Bool = op === SUB
  def isAnd(op:  AluType.Type): Bool = op === AND
  def isOr(op:   AluType.Type): Bool = op === OR
  def isNor(op:  AluType.Type): Bool = op === NOR
  def isXor(op:  AluType.Type): Bool = op === XOR
  def isSlt(op:  AluType.Type): Bool = op === SLT
  def isSltu(op: AluType.Type): Bool = op === SLTU
  def isSll(op:  AluType.Type): Bool = op === SLL
  def isSrl(op:  AluType.Type): Bool = op === SRL
  def isSra(op:  AluType.Type): Bool = op === SRA
  def isSllv(op: AluType.Type): Bool = op === SLLV
  def isSrlv(op: AluType.Type): Bool = op === SRLV
  def isSrav(op: AluType.Type): Bool = op === SRAV
  def isLui(op:  AluType.Type): Bool = op === LUI
}
