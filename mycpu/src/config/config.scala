package config

import chisel3._
import chisel3.util._

object FuType extends Enumeration {
  type t = Value
  // 自动赋值枚举成员
  val MainAlu, SubAlu, Lsu, Mdu = Value
  def needByPassIn(input:  Value) = input == MainAlu || input == SubAlu
  def needByPassOut(input: Value) = input == MainAlu || input == SubAlu
}

object BranchType extends ChiselEnum {
  val jcall, jret, jmp, jr, b, non = Value
}

object ChiselFuType extends ChiselEnum {
  val MainALU, ALU, LSU, MDU = Value
}

// I-Cahce stage1 should decode MemType and addr to LoadSel
object LoadSel extends ChiselEnum {
  val LW, LB, LBU, LH, LHU, LWL0, LWL1, LWL2, LWR1, LWR2, LWR3 = Value
}

object MduType extends ChiselEnum {
  val MULT, MULTU, DIV, DIVU, MFHI, MFLO, MTHI, MTLO, CLZ = Value
}

object CacheOp extends ChiselEnum {
  val IndexInvalidI          = Value("b00000".U)
  val IndexWriteBackInvalidD = Value("b00001".U)
  val IndexStoreTagI         = Value("b01000".U)
  val IndexStoreTagD         = Value("b01001".U)
  val HitInvalidI            = Value("b10000".U)
  val HitInvalidD            = Value("b10001".U)
  val HitWriteBackInvalidD   = Value("b10101".U)
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

object MemType extends ChiselEnum {
  val LW, LB, LBU, LH, LHU, LWL, LWR, SW, SH, SB, SWL, SWR = Value
}

object FrontExcCode extends ChiselEnum {
  val AdEL, TLBL, NONE = Value
}

trait MycpuParam {
  // General Parameter for mycpu

  val excCodeWidth       = 5
  val PaddrWidth         = 32
  val tagWidth           = 20
  val cacheIndexWidth    = 7
  val cacheOffsetWidth   = 12 - cacheIndexWidth
  val vaddrWidth         = 32
  val instrWidth         = 32
  val dataWidth          = 32
  val IcachRoads         = 4
  val DcachRoads         = 4
  val IcachLineBytes     = 32
  val DcachLineBytes     = 32
  val enableCacheInst    = true
  val memInstOffsetWidth = 16

  val predictNum  = 4
  val fetchNum    = 4
  val decodeNum   = 3
  val renameNum   = 3
  val dispatchNum = 3
  val wBNum       = 3
  val issueNum    = 2 //should be 4
  val srcDataNum  = 2
  val retireNum   = 2 //should be 4

  val aRegNum       = 32
  val aRegAddrWidth = log2Up(aRegNum)
  val pRegNum       = 64
  val pRegAddrWidth = log2Up(pRegNum)
  val robNum        = 32
  val robIndexWidth = log2Up(robNum)

  def ARegIdx = UInt(aRegAddrWidth.W)
  def PRegIdx = UInt(pRegAddrWidth.W)
  val ROBIdx  = UInt(robIndexWidth.W)

  val tlbIndexWidth = 3
  val TLBIdx        = UInt(tlbIndexWidth.W)

  val prfReadPortNum = srcDataNum * issueNum

  val aluFuNum     = 2
  val aluRsInPorts = 2

  val aluBypassNum = 2

  def UWord = UInt(32.W)
  def UByte = UInt(8.W)
  def UHalf = UInt(16.W)

  val CP0IdxWidth = 8
  def CP0Idx      = UInt(CP0IdxWidth.W)
  def CP0Idx(sel: UInt, rd: UInt) = {
    require(sel.getWidth == 3)
    require(rd.getWidth == 5)
    Cat(rd, sel)
  }
}

abstract class MycpuBundle extends Bundle with MycpuParam
abstract class MycpuModule extends Module with MycpuParam
