package backend.mem

import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import frontend.LocHisTab

object IndexPredictor {
  val mipIdxWidth: Int = 6
  val mipTagWidth: Int = 32 - mipIdxWidth - 2
  import MycpuObject.DcacheIndexWidth
  def DCacheIdx = UInt(DcacheIndexWidth.W)
  class MIPOutIO extends MycpuBundle {
    val idx = DCacheIdx
    val cnt = UInt(2.W) // use MIP idx
  }
  class WriteIO extends MycpuBundle {
    val pc       = UWord
    val wData    = new MIPOutIO
    val idxMatch = Bool()
    val tagMatch = Bool()
  }
  def getTag(address: UInt) = {
    require(address.getWidth == 32)
    address(31, 2 + mipIdxWidth)
  }
  def getIdx(address: UInt) = {
    val res = address(mipIdxWidth + 2 - 1, 2)
    require(res.getWidth == mipIdxWidth)
    res
  }
}
class IndexPredictor extends MycpuModule {
  import IndexPredictor._
  val io = IO(new Bundle {
    val readReq  = Input(UWord)
    val readRes  = Valid(new MIPOutIO)
    val writeReq = Flipped(Valid(new WriteIO))
  })
  val ramType   = UInt((mipTagWidth + DcacheIndexWidth + 2).W)
  val initValue = Cat(0.U(mipTagWidth.W), 0.U(DcacheIndexWidth.W), 1.U(2.W))
  require(initValue.getWidth == ramType.getWidth)
  val ram = Mem(math.pow(2, mipIdxWidth).toInt, ramType)

  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  val _resetState              = RegInit(true.B)
  val (_resetSet, resetFinish) = Counter(_resetState, math.pow(2, mipIdxWidth).toInt)
  when(resetFinish) { _resetState := false.B }

  resetState := _resetState
  resetSet   := _resetSet

  val wen = io.writeReq.valid || resetState

  // Read ======================================
  val rdata = ram.read(getIdx(io.readReq))
  val rTag  = rdata(ramType.getWidth - 1, DcacheIndexWidth + 2)
  val rIdx  = rdata(DcacheIndexWidth + 1, 2)
  val rCnt  = rdata(1, 0)

  require(rTag.getWidth == mipTagWidth)
  require(rIdx.getWidth == DcacheIndexWidth)
  require(rCnt.getWidth == 2)

  asg(io.readRes.valid, rTag === getTag(io.readReq))
  asg(io.readRes.bits.cnt, rCnt)
  asg(io.readRes.bits.idx, rIdx)

  // Write ======================================
  val wBits = io.writeReq.bits
  val wCnt  = Mux(wBits.tagMatch, LocHisTab.calNextCnt(wBits.wData.cnt, wBits.idxMatch), 1.U(2.W))
  val wData = Cat(getTag(wBits.pc), wBits.wData.idx, wCnt)
  require(wData.getWidth == ramType.getWidth)

  val setIdx    = Mux(resetState, resetSet, getIdx(wBits.pc))
  val wdataword = Mux(resetState, initValue, wData)
  when(wen) { ram.write(setIdx, wdataword) }
}
