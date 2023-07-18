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
  val NON                    = Value
  def isC0Inst(op: MduType.Type): Bool = op === MTC0 || op === MFC0
}
object SpecialType extends ChiselEnum {
  val LOAD, STORE, MTC0, MTHI, MTLO, MULDIV, ERET, CACHEINST, HB, NON = Value
}

//not need now
object BlockType extends ChiselEnum {
  val CACHEINST, SYNC, MFC0, NON = Value
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
  //cacheinst
  val CACHEINST = Value("b0110".U)
  val NON       = Value("b0111".U)
  // group byte0
  val LB  = Value("b1000".U)
  val LBU = Value("b1001".U)
  val SB  = Value("b1010".U)
  // group half
  val LH  = Value("b1100".U)
  val LHU = Value("b1101".U)
  val SH  = Value("b1110".U)

  val LL = Value("b10000".U)
  val SC = Value("b10001".U)

  def wordPat  = BitPat("b?000?")
  def leftPat  = BitPat("b0001?")
  def rightPat = BitPat("b0010?")
  def bytePat  = BitPat("b010??")
  def halfPat  = BitPat("b011??")
  def isLoad(op: MemType.Type) = {
    op.isOneOf(LW, LL, LB, LBU, LH, LHU, LWL, LWR)
  }
  def isStore(op: MemType.Type) = {
    op.isOneOf(SW, SC, SWL, SWR, SB, SH)
  }
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
  //trans
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
  def isBr(op: BranchType.Type): Bool = op =/= NON
}
object AluType extends ChiselEnum {
  val ADD, ADDI, ADDU, ADDIU, SUB, SUBU, AND, ANDI, OR, ORI, XOR, XORI, NOR, SLT, SLTI, SLTU, SLTIU, SLL, SRL, SRA,
    SLLV, SRLV, SRAV, LUI, MOVN, MOVZ, NON = Value
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
  def zeroExt(op: AluType.Type): Bool =
    op === ANDI || op === ORI || op === XORI
  def mayOverflow(op: AluType.Type): Bool = op === ADD || op === ADDI || op === SUB
  def isSll(op:       AluType.Type): Bool = op === SLL
  def isSrl(op:       AluType.Type): Bool = op === SRL
  def isSra(op:       AluType.Type): Bool = op === SRA
  def needSa(op:      AluType.Type): Bool = isSll(op) || isSrl(op) || isSra(op)
}
object DeExType extends ChiselEnum {
  val RI, SYS, BP, NON = Value
}

object FuType extends Enumeration {
  type t = Value
  // 自动赋值枚举成员
  val MainAlu, SubAlu, Lsu, Mdu = Value
  def needByPassIn(input:  Value) = input == MainAlu || input == SubAlu
  def needByPassOut(input: Value) = input == MainAlu || input == SubAlu
}

object BtbType extends ChiselEnum {
  val jcall  = Value("b000".U)
  val jret   = Value("b001".U)
  val jmp    = Value("b010".U)
  val jr     = Value("b011".U)
  val b, non = Value
  def isJump(brType: BtbType.Type) = !brType.asUInt(2).asBool
}

object CCAttr extends ChiselEnum {
  val zero     = Value("b000".U)
  val Uncached = Value("b010".U)
  val Cached   = Value("b011".U)
  val other    = Value("b100".U)
  def isUnCache(attr: UInt) = attr =/= Cached.asUInt
}

object CacheOp extends ChiselEnum {
  val IndexInvalidI          = Value("b00000".U)
  val IndexWriteBackInvalidD = Value("b00001".U)
  val IndexStoreTagI         = Value("b01000".U)
  val IndexStoreTagD         = Value("b01001".U)
  val HitInvalidI            = Value("b10000".U)
  val HitInvalidD            = Value("b10001".U)
  val HitWriteBackInvalidD   = Value("b10101".U)
  def isDop(op: CacheOp.Type) = {
    op.asUInt(4) || (op.asUInt === 1.U)
  }
  def isIop(op: CacheOp.Type) = {
    !isDop(op)
  }
  def isIdxInv(op: CacheOp.Type) = {
    op.asUInt < 2.U
  }
  def isIdxStoreTag(op: CacheOp.Type) = {
    op.asUInt(4, 1) === "b0100".U
  }
  def isHitInv(op: CacheOp.Type) = {
    op.asUInt(4)
  }
}

class ExcCode extends MycpuBundle {
  val excCode = ExcCode()
  val refill  = Bool()
}

object ExcCode extends ChiselEnum {
  val Int  = Value(0x00.U)
  val Mod  = Value(0x01.U)
  val TLBL = Value(0x02.U)
  val TLBS = Value(0x03.U)
  val AdEL = Value(0x04.U)
  val AdES = Value(0x05.U)
  val IBE  = Value(0x06.U)
  val DBE  = Value(0x07.U)
  val Sys  = Value(0x08.U)
  val Bp   = Value(0x09.U)
  val RI   = Value(0x0a.U)
  val CpU  = Value(0x0b.U)
  val Ov   = Value(0x0c.U)
  val Tr   = Value(0x0d.U)
}

object FrontExcCode extends ChiselEnum {
  val AdEL, InvalidTLBL, RefillTLBL, NONE = Value
  def happen(code:   FrontExcCode.Type): Bool         = code =/= NONE
  def isRefill(code: FrontExcCode.Type): Bool         = code === RefillTLBL
  def trans(code:    FrontExcCode.Type): ExcCode.Type = Mux(code === AdEL, ExcCode.AdEL, ExcCode.TLBL)
}
