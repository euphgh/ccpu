package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
import cache._
import utils._
import utils.BytesWordUtils._

/**
  * pass abort signal to cacheStage2 in this stage:in.wbRob.exception
  *   if exception happen,dont do anything
  *
  * pay attention
  * 1. Mem2 won't be block by WB conflict(block mdu)
  * 2. ready can not be set from in.dCache.cacheInst.valid to cache redirect
  *
  * instantiate cacheStage2 here,give it
  *    ptag
  *    isUncache
  *    fromStage1
  *    isException
  * when miss,use DramReadIO to connect Dram
  */
class MemStage2 extends MycpuModule {
  val io = IO(new Bundle {
    val in      = Flipped(Decoupled(new MemStage1OutIO))
    val out     = Decoupled(new FunctionUnitOutIO)
    val querySQ = new QuerySQ
    val doneSQ  = Output(Bool()) //connect storeQ deq.back
    val donePC  = if (debug) Some(Output(UWord)) else None //connect storeQ deq.back
    val dmem    = new DramIO
    val flush   = Input(Bool()) // for cache instr
  })
  val outBits    = io.out.bits
  val inBits     = io.in.bits
  val prevDstSrc = inBits.prevDstSrc
  outBits.wbRob.robIndex     := inBits.wbInfo.robIndex
  outBits.wbRob.isMispredict := false.B
  outBits.wbRob.exDetect     := inBits.exDetect
  outBits.wPrf.pDest         := inBits.wbInfo.destPregAddr
  outBits.destAregAddr       := inBits.wbInfo.destAregAddr
  if (debug) asg(outBits.wbRob.debugPC.get, inBits.debugPC.get)
  // ======================  Cache ============================
  val cache2  = Module(new CacheStage2(DcachRoads, DcachLineBytes, true)())
  val cinBit  = cache2.io.in.bits
  val coutBit = cache2.io.out.bits
  asg(cinBit.fromStage1, inBits.toCache2)
  asg(cinBit.ptag, inBits.pTag)
  asg(cinBit.isUncached, inBits.isUncache)
  import MemType._
  val isCi       = if (enableCacheInst) io.in.bits.toCache2.cacheInst.get.valid else false.B
  val isld       = !inBits.isSQ && isLoad(inBits.memType)
  val cacheMask  = Mux(inBits.isUncache, io.querySQ.req.needMask, io.querySQ.res.memMask)
  val ldHitSQ    = !cacheMask.orR // not write and mem mask==0.U
  val inIndex    = inBits.toCache2.dCacheReq.get.lowAddr.index
  val cancelUart = inBits.pTag === "h1fe40".U && inIndex === 0.U(cacheIndexWidth.W) && inBits.isUncache === false.B
  asg(cinBit.cancel, inBits.exDetect.happen || (ldHitSQ || cancelUart) && isld)
  // store req from rostage should not enter cache
  cache2.io.in.valid := io.in.valid
  io.dmem <> cache2.io.dram
  io.in.ready := cache2.io.in.ready && io.out.ready // LSU is always priori to MDU
  val cacheFinish = cache2.io.out.valid
  // store req from rostage should not wait cache
  io.out.valid        := Mux(isld, cacheFinish, if (enableCacheInst) cache2.io.cacheInst.finish.get else false.B)
  io.doneSQ           := cacheFinish && inBits.isSQ
  cache2.io.out.ready := Mux(inBits.isSQ, true.B, io.out.ready)

  if (debug) io.donePC.get := inBits.debugPC.get
  // ===================== select ===============================
  val lowAddr = inBits.toCache2.dCacheReq.get.lowAddr
  asg(io.querySQ.req.addr, Cat(inBits.pTag, lowAddr.index, lowAddr.offset))
  asg(io.querySQ.req.needMask, inBits.toCache2.dCacheReq.get.wStrb)
  val validWord  = maskWord(coutBit.ddata.get, cacheMask).asUInt | maskWord(io.querySQ.res.data, ~cacheMask).asUInt
  val validBytes = word2Bytes(validWord)
  val l2sb       = inBits.toCache2.dCacheReq.get.lowAddr.offset(1, 0)
  // >> align ====================================================
  // >> >> bytes =================================================
  val byteRes = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> validBytes(i)
    })
  )
  val lb  = SignExt(byteRes, 32)
  val lbu = ZeroExt(byteRes, 32)
  // >> >> half ====================================================
  val halfRes = Mux(l2sb(1), Cat(validBytes(3), validBytes(2)), Cat(validBytes(1), validBytes(0)))
  val lh      = SignExt(halfRes, 32)
  val lhu     = ZeroExt(halfRes, 32)
  // >> >> word ====================================================
  val lw = validWord
  // >> not align ==================================================
  val lwl = LookupUInt(
    l2sb,
    Seq(
      0.U -> Cat(validBytes(0), prevDstSrc(2), prevDstSrc(1), prevDstSrc(0)),
      1.U -> Cat(validBytes(1), validBytes(0), validBytes(1), prevDstSrc(0)),
      2.U -> Cat(validBytes(2), validBytes(1), validBytes(0), prevDstSrc(0)),
      3.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0))
    )
  )
  val lwr = LookupUInt(
    l2sb,
    Seq(
      0.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0)),
      1.U -> Cat(prevDstSrc(3), validBytes(3), validBytes(2), validBytes(1)),
      2.U -> Cat(prevDstSrc(3), prevDstSrc(2), validBytes(3), validBytes(2)),
      3.U -> Cat(prevDstSrc(3), prevDstSrc(2), prevDstSrc(1), validBytes(3))
    )
  )
  asg(
    io.out.bits.wPrf.result,
    LookupEnum(
      inBits.memType,
      Seq(
        MemType.LB  -> lb,
        MemType.LBU -> lbu,
        MemType.LH  -> lh,
        MemType.LHU -> lhu,
        MemType.LW  -> lw,
        MemType.LWL -> lwl,
        MemType.LWR -> lwr
      )
    )
  )
  asg(io.out.bits.wPrf.wmask, Mux(isld, "b1111".U(4.W), 0.U))
  // CacheInst =====================================================
  if (enableCacheInst) {
    val inci = io.in.bits.toCache2.cacheInst.get
    when(io.in.valid) {
      assert(!inBits.isSQ || !inci.valid)
    }
    when(inci.valid) {
      io.out.valid  := cache2.io.cacheInst.finish.get
      cinBit.cancel := false.B
    }
    asg(cache2.io.cacheInst.redirect.get, io.flush)
  }
}
