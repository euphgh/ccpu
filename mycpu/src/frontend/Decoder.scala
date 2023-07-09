package frontend

import config._
import chisel3._
import chisel3.util._
import decodemacro.MacroDecode

object AllInsts {
  def apply(): Seq[Tuple2[chisel3.util.BitPat, List[chisel3.ChiselEnum#Type]]] = {
    Seq(
      ADD   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.ADD),
      ADDU  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.ADDU),
      SUB   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SUB),
      SUBU  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SUBU),
      AND   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.AND),
      OR    -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.OR),
      XOR   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.XOR),
      NOR   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.NOR),
      SLT   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SLT),
      SLTU  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SLTU),
      SLL   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SLL),
      SRL   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SRL),
      SRA   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SRA),
      SLLV  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SLLV),
      SRLV  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SRLV),
      SRAV  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.SubALU, AluType.SRAV),
      MULT  -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.MULT, SpecialType.MULDIV),
      MULTU -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.MULTU, SpecialType.MULDIV),
      DIV   -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.DIV, SpecialType.MULDIV),
      DIVU  -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.DIVU, SpecialType.MULDIV),
      MFHI  -> List(SRCType.noSRC, DSTType.toRD, ChiselFuType.MDU, MduType.MFHI),
      MFLO  -> List(SRCType.noSRC, DSTType.toRD, ChiselFuType.MDU, MduType.MFLO),
      MTHI  -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MDU, MduType.MTHI, SpecialType.MTHI),
      MTLO  -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MDU, MduType.MTLO, SpecialType.MTLO),
      LB    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU, MemType.LB, SpecialType.LOAD),
      LBU   -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU, MemType.LBU, SpecialType.LOAD),
      LH    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU, MemType.LH, SpecialType.LOAD),
      LHU   -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU, MemType.LHU, SpecialType.LOAD),
      LW    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU, MemType.LW, SpecialType.LOAD),
      LWL   -> List(SRCType.RSRT, DSTType.toRT, ChiselFuType.LSU, MemType.LWL, SpecialType.LOAD),
      LWR   -> List(SRCType.RSRT, DSTType.toRT, ChiselFuType.LSU, MemType.LWR, SpecialType.LOAD),
      SB    -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.SB, SpecialType.STORE),
      SH    -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.SH, SpecialType.STORE),
      SW    -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.SW, SpecialType.STORE),
      SWR   -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.SWR, SpecialType.STORE),
      SWL   -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.SWL, SpecialType.STORE),
      // U type
      ADDI    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.ADDI),
      ADDIU   -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.ADDIU),
      SLTI    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.SLTI),
      SLTIU   -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.SLTIU),
      ANDI    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.ANDI),
      ORI     -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.ORI),
      XORI    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.XORI),
      LUI     -> List(SRCType.RS, DSTType.toRT, ChiselFuType.SubALU, AluType.LUI),
      MFC0    -> List(SRCType.noSRC, DSTType.toRT, ChiselFuType.MDU, MduType.MFC0),
      MTC0    -> List(SRCType.RT, DSTType.noDST, ChiselFuType.MDU, MduType.MTC0, SpecialType.MTC0),
      BREAK   -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU, DeExType.BP),
      SYSCALL -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU, DeExType.SYS),
      ERET    -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU, SpecialType.ERET),
      BEQ     -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MainALU, BranchType.BEQ),
      BNE     -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MainALU, BranchType.BNE),
      BGEZ    -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.BGEZ),
      BLEZ    -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.BLEZ),
      BLTZ    -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.BLTZ),
      BGTZ    -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.BGTZ),
      BLTZAL  -> List(SRCType.RS, DSTType.to31, ChiselFuType.MainALU, BranchType.BLTZAL),
      BGEZAL  -> List(SRCType.RS, DSTType.to31, ChiselFuType.MainALU, BranchType.BGEZAL),
      //J type
      J    -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU, BranchType.J),
      JAL  -> List(SRCType.noSRC, DSTType.to31, ChiselFuType.MainALU, BranchType.JAL),
      JR   -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.JR),
      JALR -> List(SRCType.RS, DSTType.toRD, ChiselFuType.MainALU, BranchType.JALR),
      // for uboot
      MUL   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.MDU, MduType.MUL),
      MOVN  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.MainALU), //TODO:
      MOVZ  -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.MainALU), //TODO:
      CACHE -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU, MemType.CACHEINST, SpecialType.CACHEINST),
      SYNC  -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      JRHB  -> List(SRCType.RS, DSTType.noDST, ChiselFuType.MainALU, BranchType.JRHB, SpecialType.HB),
      // for linux
      MADD  -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.MADD),
      MADDU -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.MADDU),
      MSUB  -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MDU, MduType.MSUB),
      TLBP  -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      TLBR  -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      TLBWI -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      TLBWR -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      LL    -> List(SRCType.RS, DSTType.toRT, ChiselFuType.LSU),
      SC    -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.LSU),
      TNE   -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MainALU),
      TEQ   -> List(SRCType.RSRT, DSTType.noDST, ChiselFuType.MainALU),
      CLZ   -> List(SRCType.RSRT, DSTType.toRD, ChiselFuType.MDU, MduType.CLZ),
      PREF  -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU),
      WAIT  -> List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU)
    )
  }
  def default(): List[chisel3.ChiselEnum#Type] = {
    List(SRCType.noSRC, DSTType.noDST, ChiselFuType.MainALU, DeExType.RI)
  }

  def ADD   = BitPat("b000000_?????___?????   ?????   00000  100000")
  def ADDU  = BitPat("b000000_?????___?????   ?????   00000  100001")
  def SUB   = BitPat("b000000_?????___?????   ?????   00000  100010")
  def SUBU  = BitPat("b000000_?????___?????   ?????   00000  100011")
  def AND   = BitPat("b000000_?????___?????   ?????   00000  100100")
  def OR    = BitPat("b000000_?????___?????   ?????   00000  100101")
  def XOR   = BitPat("b000000_?????___?????   ?????   00000  100110")
  def NOR   = BitPat("b000000_?????___?????   ?????   00000  100111")
  def SLT   = BitPat("b000000_?????___?????   ?????   00000  101010")
  def SLTU  = BitPat("b000000_?????___?????   ?????   00000  101011")
  def SLL   = BitPat("b000000_00000___?????   ?????   ?????  000000")
  def SRL   = BitPat("b000000_00000___?????   ?????   ?????  000010")
  def SRA   = BitPat("b000000_00000___?????   ?????   ?????  000011")
  def SLLV  = BitPat("b000000_?????___?????   ?????   00000  000100")
  def SRLV  = BitPat("b000000_?????___?????   ?????   00000  000110")
  def SRAV  = BitPat("b000000_?????___?????   ?????   00000  000111")
  def MULT  = BitPat("b000000_?????___?????   00000   00000  011000")
  def MULTU = BitPat("b000000_?????___?????   00000   00000  011001")
  def DIV   = BitPat("b000000_?????___?????   00000   00000  011010")
  def DIVU  = BitPat("b000000_?????___?????   00000   00000  011011")
  def MFHI  = BitPat("b000000_00000___00000   ?????   00000  010000")
  def MFLO  = BitPat("b000000_00000___00000   ?????   00000  010010")

  //I type
  //      opcode  rs      rt      imm
  //      6       5       5       16
  //      [31:26] [25:21] [20:16] [15:0]
  def ADDI  = BitPat("b001000_?????   ?????   ????? ????? ??????")
  def ADDIU = BitPat("b001001_?????   ?????   ????? ????? ??????")
  def SLTI  = BitPat("b001010_?????   ?????   ????? ????? ??????")
  def SLTIU = BitPat("b001011_?????   ?????   ????? ????? ??????")

  def LB  = BitPat("b100000_?????   ?????   ????? ????? ??????")
  def LWL = BitPat("b100010_?????   ?????   ????? ????? ??????")
  def LBU = BitPat("b100100_?????   ?????   ????? ????? ??????")
  def LWR = BitPat("b100110_?????   ?????   ????? ????? ??????")
  def LH  = BitPat("b100001_?????   ?????   ????? ????? ??????")
  def LW  = BitPat("b100011_?????   ?????   ????? ????? ??????")
  def LHU = BitPat("b100101_?????   ?????   ????? ????? ??????")

  def SB  = BitPat("b101000_?????   ?????   ????? ????? ??????")
  def SH  = BitPat("b101001_?????   ?????   ????? ????? ??????")
  def SWL = BitPat("b101010_?????   ?????   ????? ????? ??????")
  def SW  = BitPat("b101011_?????   ?????   ????? ????? ??????")
  def SWR = BitPat("b101110_?????   ?????   ????? ????? ??????")

  // U type
  //                   opcode  rs      rt      imm
  //                   6       5       5       16
  //                   [31:26] [25:21] [20:16] [15:0]
  def ANDI = BitPat("b001100  ?????   ?????   ????? ????? ??????")
  def ORI  = BitPat("b001101  ?????   ?????   ????? ????? ??????")
  def XORI = BitPat("b001110  ?????   ?????   ????? ????? ??????")
  def LUI  = BitPat("b001111  00000   ?????   ????? ????? ??????")
  def MTHI = BitPat("b000000  ?????   00000   00000 00000 010001")
  def MTLO = BitPat("b000000  ?????   00000   00000 00000 010011")
  def MFC0 = BitPat("b010000  00000   ?????   ????? 00000 000???")
  def MTC0 = BitPat("b010000  00100   ?????   ????? 00000 000???")

  def BREAK   = BitPat("b000000_?????   ?????   ????? ????? 001101")
  def SYSCALL = BitPat("b000000_?????   ?????   ????? ????? 001100")
  def ERET    = BitPat("b010000_10000   00000   00000 00000 011000")

  //B type
  //                   opcode  rs      rt      imm
  //                   6       5       5       16
  //                   [31:26] [25:21] [20:16] [15:0]
  def BEQ    = BitPat("b000100_?????   ?????   ????? ????? ??????")
  def BNE    = BitPat("b000101_?????   ?????   ????? ????? ??????")
  def BLTZ   = BitPat("b000001_?????   00000   ????? ????? ??????")
  def BLTZAL = BitPat("b000001_?????   10000   ????? ????? ??????")
  def BGEZ   = BitPat("b000001_?????   00001   ????? ????? ??????")
  def BGEZAL = BitPat("b000001_?????   10001   ????? ????? ??????")
  def BGTZ   = BitPat("b000111_?????   00000   ????? ????? ??????")
  def BLEZ   = BitPat("b000110_?????   00000   ????? ????? ??????")

  //J type
  //      opcode  rs      0       rd
  //      6       5       5       16
  //      [31:26] [25:21] [20:16] [15:11]
  def J    = BitPat("b000010 ?????   ?????   ????? ????? ??????")
  def JAL  = BitPat("b000011 ?????   ?????   ????? ????? ??????")
  def JR   = BitPat("b000000 ?????   00000   00000 00000 001000")
  def JALR = BitPat("b000000 ?????   00000   ????? 00000 001001")

  // for uboot
  def MUL   = BitPat("b011100 ?????   ?????   ????? 00000 000010")
  def MOVN  = BitPat("b000000 ?????   ?????   ????? 00000 001011")
  def MOVZ  = BitPat("b000000 ?????   ?????   ????? 00000 001010")
  def CACHE = BitPat("b101111 ?????   ?????   ????? ????? ??????")
  def SYNC  = BitPat("b000000 00000   00000   00000 ????? 001111")
  def JRHB  = BitPat("b000000 ?????   00000   00000 1???? 001000")
  // for linux
  def MADD  = BitPat("b011100 ?????   ?????   00000 00000 000000")
  def MADDU = BitPat("b011100 ?????   ?????   00000 00000 000001")
  def MSUB  = BitPat("b011100 ?????   ?????   00000 00000 000100")
  def TLBP  = BitPat("b010000 10000   00000   00000 00000 001000")
  def TLBR  = BitPat("b010000 10000   00000   00000 00000 000001")
  def TLBWI = BitPat("b010000 10000   00000   00000 00000 000010")
  def TLBWR = BitPat("b010000 10000   00000   00000 00000 000110")
  def LL    = BitPat("b110000 ?????   ?????   ????? ????? ??????")
  def SC    = BitPat("b111000 ?????   ?????   ????? ????? ??????")
  def TNE   = BitPat("b000000 ?????   ?????   ????? ????? 110110")
  def TEQ   = BitPat("b000000 ?????   ?????   ????? ????? 110100")
  def CLZ   = BitPat("b011100 ?????   ?????   ????? 00000 100000")
  def PREF  = BitPat("b110011 ?????   ?????   ????? ????? ??????")
  def WAIT  = BitPat("b010000 1????   ?????   ????? ????? 100000")
  // RI exception
  def RI_011 = BitPat("b011??? ?????   ?????   ????? ????? ??????")
  def RI_BL  = BitPat("b0101?? ?????   ?????   ????? ????? ??????")
  def RI_FT  = BitPat("b010001 01110   11111   00000 00011 100000")
  def RI_COP = BitPat("b010??? ?????   ?????   ????? ????? ??????")
  def RI_47  = BitPat("b100111 ?????   ?????   ????? ????? ??????")
  def RI_101 = BitPat("b10110? ?????   ?????   ????? ????? ??????")
  def RI_11X = BitPat("b11???? ?????   ?????   ????? ????? ??????")
}
