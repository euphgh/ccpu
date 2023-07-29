package config
import chisel3._
import chisel3.util._

trait MycpuParam {
  // General Parameter for mycpu

  val excCodeWidth     = 5
  val PaddrWidth       = 32
  val tagWidth         = 20
  val cacheIndexWidth  = 7
  val cacheOffsetWidth = 12 - cacheIndexWidth
  val vaddrWidth       = 32
  val instrWidth       = 32
  val dataWidth        = 32
  val IcachRoads       = 4
  val DcachRoads       = 4
  val IcachLineBytes   = 32
  val DcachLineBytes   = 32
  val enableCacheInst  = true
  val immWidth         = 16
  val retAddrStackSize = 8
  val storeQSize       = 4
  def getAddrIdx(word: UInt) = word(cacheIndexWidth + cacheOffsetWidth - 1, cacheOffsetWidth)
  def getOffset(word:  UInt) = word(cacheOffsetWidth - 1, 0)
  val instrOffLsb   = 2
  val instrOffMsb   = log2Ceil(IcachLineBytes) - 1
  val instrOffWidth = instrOffMsb - instrOffLsb + 1

  val predictNum  = 4
  val fetchNum    = 4
  val decodeNum   = 3
  val renameNum   = 3
  val dispatchNum = 3
  val wBNum       = 3
  val issueNum    = 4 //should be 4
  val srcDataNum  = 2
  val retireNum   = 2 //should be 4

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

  val tlbEntriesNum = 4
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
}

abstract class MycpuBundle extends Bundle with MycpuProperties
abstract class MycpuModule extends Module with MycpuProperties
object MycpuObject extends MycpuProperties
