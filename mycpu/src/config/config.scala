package config
import chisel3._
import chisel3.util._

trait MycpuParam {
  // configurable:
  val IcachLineBytes   = 64
  val DcachLineBytes   = 64
  val basicBpuIdxWidth = 6

  val IcachRoads       = 4
  val DcachRoads       = 4
  val retAddrStackSize = 8
  val storeQSize       = 8
  val tlbEntriesNum    = 4
  // General Parameter for mycpu
  val excCodeWidth = 5
  val PaddrWidth   = 32
  val tagWidth     = 20
  require(IcachLineBytes == 64 || IcachLineBytes == 32)
  require(DcachLineBytes == 64 || DcachLineBytes == 32)
  val IcacheOffsetWidth = log2Ceil(IcachLineBytes)
  val DcacheOffsetWidth = log2Ceil(DcachLineBytes)
  val IcacheIndexWidth  = 12 - IcacheOffsetWidth
  val DcacheIndexWidth  = 12 - DcacheOffsetWidth
  val vaddrWidth        = 32
  val instrWidth        = 32
  val dataWidth         = 32
  val enableCacheInst   = true
  val immWidth          = 16
  def getAddrIdxI(word: UInt) = word(IcacheIndexWidth + IcacheOffsetWidth - 1, IcacheOffsetWidth)
  def getAddrIdxD(word: UInt) = word(DcacheIndexWidth + DcacheOffsetWidth - 1, DcacheOffsetWidth)
  def getOffsetI(word:  UInt) = word(IcacheOffsetWidth - 1, 0)
  def getOffsetD(word:  UInt) = word(DcacheOffsetWidth - 1, 0)
  def isDataId(id:      UInt) = id(0)
  def isInstrId(id:     UInt) = id(0) =/= false.B
  def isDCacheId(id:    UInt) = id === 1.U
  def isUartBufId(id:   UInt) = id === 3.U
  val instrOffLsb   = 2
  val instrOffMsb   = log2Ceil(IcachLineBytes) - 1
  val instrOffWidth = instrOffMsb - instrOffLsb + 1
  def getAlignPC(pc: UInt) = {
    require(pc.getWidth == 32)
    val ifTag = pc(31, 4)
    val alignPC =
      Mux(
        pc(instrOffMsb, instrOffLsb) > ((IcachLineBytes / 4) - 4).U(instrOffWidth.W),
        Cat(ifTag + 1.U, 0.U(4.W)),
        Cat(ifTag + 1.U, pc(3, 0))
      )
    alignPC
  }

  val predictNum  = 4
  val fetchNum    = 4
  val decodeNum   = 3
  val renameNum   = 3
  val dispatchNum = 3
  val wBNum       = 3
  val issueNum    = 4 //should be 4
  val srcDataNum  = 2
  val retireNum   = 3 //should be 4

  val aRegNum       = 32
  val aRegAddrWidth = log2Up(aRegNum)
  val pRegNum       = 64
  val pRegAddrWidth = log2Up(pRegNum)
  val robNum        = 32
  val robIndexWidth = log2Up(robNum)
  val freeListSize  = 32

  def ARegIdx = UInt(aRegAddrWidth.W)
  def PRegIdx = UInt(pRegAddrWidth.W)
  def ROBIdx  = UInt(robIndexWidth.W)

  val tlbIndexWidth = log2Ceil(tlbEntriesNum)
  def TLBIdx        = UInt(tlbIndexWidth.W)

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

  val maOBpNum  = FuType.oBpNum(FuType.MainAlu)
  val saOBpNum  = FuType.oBpNum(FuType.SubAlu)
  val lsuOBpNum = FuType.oBpNum(FuType.Lsu)
  val mduOBpNum = FuType.oBpNum(FuType.Mdu)
}

abstract class MycpuBundle extends Bundle with MycpuProperties
abstract class MycpuModule extends Module with MycpuProperties
object MycpuObject extends MycpuProperties
