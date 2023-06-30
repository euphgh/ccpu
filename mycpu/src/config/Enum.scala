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
  // block
  val MULT, MULTU, MUL         = Value
  val MADD, MADDU, MSUB, MSUBU = Value
  val DIV, DIVU                = Value
  val CLZ                      = Value
  val TLBP                     = Value
  // through
  val MFHI, MFLO, MTHI, MTLO = Value
  val MTC0, MFC0             = Value
  val TLBR, TLBWI, TLBWR     = Value
  def isC0Inst(op: MduType.Type): Bool = op === MTC0 || op === MFC0
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
  //cacheinst
  val CACHEINST = Value("b0110".U)

  def wordPat  = BitPat("b000?")
  def leftPat  = BitPat("b001?")
  def rightPat = BitPat("b010?")
  def bytePat  = BitPat("b10??")
  def halfPat  = BitPat("b11??")
}
object BranchType extends ChiselEnum {
  val BEQ, BNE, BGEZ, BLEZ, BLTZ, BGTZ, BLTZAL, BGEZAL, J, JAL, JR, JALR, JRHB, NON = Value
  def isJr(op: BranchType.Type): Bool = op === JR || op === JALR || op === JRHB
  def isJ(op:  BranchType.Type): Bool = op === J || op === JAL
  def isB(op:  BranchType.Type): Bool =
    op === BEQ || op === BNE || op === BGEZ || op === BLEZ || op === BLTZ || op === BGTZ || op === BLTZAL || op === BGEZAL
  def isAL(op: BranchType.Type): Bool =
    op === BLTZAL || op === BGEZAL || op === JAL || op === JALR
  //cond
  def eq(op:     BranchType.Type): Bool = op === BEQ
  def ne(op:     BranchType.Type): Bool = op === BNE
  def gez(op:    BranchType.Type): Bool = op === BGEZ || op === BGEZAL
  def gtz(op:    BranchType.Type): Bool = op === BGTZ
  def lez(op:    BranchType.Type): Bool = op === BLEZ
  def ltz(op:    BranchType.Type): Bool = op === BLTZ || op === BLTZAL
  def noCond(op: BranchType.Type): Bool = isJ(op) || isJr(op)

  def toBtbType(op: BranchType.Type, rs: UInt): BtbType.Type =
    MuxCase(
      BtbType.non,
      Seq(
        isB(op)                -> BtbType.b,
        isAL(op)               -> BtbType.jcall, //非B类AL: jal jalr
        (op === JR && rs.andR) -> BtbType.jret, //rs===31
        (op === JR)            -> BtbType.jr, //not jr 31
        (op === J)             -> BtbType.jmp
      )
    )
}
object AluType extends ChiselEnum {
  val ADD, ADDI, ADDU, ADDIU, SUB, SUBU, AND, ANDI, OR, ORI, XOR, XORI, NOR, SLT, SLTI, SLTU, SLTIU, SLL, SRL, SRA,
    SLLV, SRLV, SRAV, LUI, NON = Value
  def useAdd(op:  AluType.Type): Bool = op === ADD || op === ADDI || op === ADDU || op === ADDIU
  def useSub(op:  AluType.Type): Bool = op === SUB || op === SUBU
  def useAnd(op:  AluType.Type): Bool = op === AND || op === ANDI
  def useOr(op:   AluType.Type): Bool = op === OR || op === ORI
  def useXor(op:  AluType.Type): Bool = op === XOR || op === XORI
  def useNor(op:  AluType.Type): Bool = op === NOR
  def useSlt(op:  AluType.Type): Bool = op === SLT || op === SLTI
  def useSltu(op: AluType.Type): Bool = op === SLTU || op === SLTIU
  def useSll(op:  AluType.Type): Bool = op === SLL || op === SLLV
  def useSrl(op:  AluType.Type): Bool = op === SRL || op === SRLV
  def useSra(op:  AluType.Type): Bool = op === SRA || op === SRAV
  def useLui(op:  AluType.Type): Bool = op === LUI

  def useImm(op: AluType.Type): Bool =
    op === ADDI || op === ADDIU || op === ANDI || op === ORI || op === XORI || op === SLTI || op === SLTIU || op === LUI
  def mayOverflow(op: AluType.Type): Bool = op === ADD || op === ADDI || op === SUB
  def isSll(op:       AluType.Type): Bool = op === SLL
  def isSrl(op:       AluType.Type): Bool = op === SRL
  def isSra(op:       AluType.Type): Bool = op === SRA
  def needSa(op:      AluType.Type): Bool = isSll(op) || isSrl(op) || isSra(op)
}
object DeExType extends ChiselEnum {
  val RI, SYS, BP, NON = Value
}
