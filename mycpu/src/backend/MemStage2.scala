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
    val dmem    = new DramIO
    val flush   = Input(Bool()) // for cache instr
  })
  val outBits = io.out.bits
  val inBits  = io.in.bits
  outBits.wbRob.robIndex     := inBits.wbInfo.robIndex
  outBits.wbRob.isMispredict := false.B
  outBits.wbRob.exDetect     := inBits.exDetect
  outBits.wPrf.pDest         := inBits.wbInfo.destPregAddr
  outBits.destAregAddr       := inBits.wbInfo.destAregAddr
  if (debug) asg(outBits.debugPC.get, inBits.debugPC.get)
  // ======================  Cache ============================
  val cache2  = Module(new CacheStage2(DcachRoads, DcachLineBytes, true)())
  val cinBit  = cache2.io.in.bits
  val coutBit = cache2.io.out.bits
  asg(cinBit.fromStage1, inBits.toCache2)
  asg(cinBit.ptag, inBits.pTag)
  asg(cinBit.cancel, inBits.exDetect.happen)
  asg(cinBit.isUncached, inBits.isUncache)
  asg(cinBit.cancel, io.querySQ.res.memMask === 0.U) // storeQ find
  cache2.io.in.valid := io.in.valid
  cache2.io.in.valid := io.in.valid
  io.dmem <> cache2.io.dram
  io.in.ready := cache2.io.in.ready && io.out.ready // LSU is always priori to MDU
  val cacheFinish = io.in.valid && cache2.io.out.valid
  io.out.valid        := cacheFinish && !inBits.isSQ
  io.doneSQ           := cacheFinish && inBits.isSQ
  cache2.io.out.ready := io.out.ready
  assert(inBits.isSQ ^ inBits.toCache2.cacheInst.get.valid)
  // ===================== select ===============================
  val lowAddr = inBits.toCache2.dCacheReq.get.lowAddr
  asg(io.querySQ.req.addr, Cat(inBits.pTag, lowAddr.index, lowAddr.offset))
  asg(io.querySQ.req.needMask, inBits.toCache2.dCacheReq.get.wStrb)
  val validWord  = maskWord(coutBit.ddata.get, io.querySQ.res.memMask).asUInt | io.querySQ.res.data
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
  val halfRes = Mux(l2sb(0), Cat(validBytes(1), validBytes(0)), Cat(validBytes(3), validBytes(2)))
  val lh      = SignExt(halfRes, 32)
  val lhu     = ZeroExt(halfRes, 32)
  // >> >> word ====================================================
  val lw = validWord
  // >> not align ==================================================
  val lwl = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> Cat(validBytes(i), validBytes((3 + i) % 4), validBytes((2 + i) % 4), validBytes((1 + i) % 4))
    })
    // Seq(
    //   0.U -> Cat(validBytes(0), validBytes(3), validBytes(2), validBytes(1)),
    //   1.U -> Cat(validBytes(1), validBytes(0), validBytes(3), validBytes(2)),
    //   2.U -> Cat(validBytes(2), validBytes(1), validBytes(0), validBytes(3)),
    //   3.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0))
    // )
  )
  val lwr = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> Cat(validBytes((3 + i) % 4), validBytes((2 + i) % 4), validBytes((1 + i) % 4), validBytes(i))
    })
    // Seq(
    //   0.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0)),
    //   1.U -> Cat(validBytes(0), validBytes(3), validBytes(2), validBytes(1)),
    //   2.U -> Cat(validBytes(1), validBytes(0), validBytes(3), validBytes(2)),
    //   3.U -> Cat(validBytes(2), validBytes(1), validBytes(0), validBytes(3))
    // )
  )
  val lwrWen = LookupUInt(
    l2sb,
    Seq(
      0.U -> "b1111".U,
      1.U -> "b0111".U,
      2.U -> "b0011".U,
      3.U -> "b0001".U
    )
  )
  val lwlWen = LookupUInt(
    l2sb,
    Seq(
      0.U -> "b1000".U,
      1.U -> "b1100".U,
      2.U -> "b1110".U,
      3.U -> "b1111".U
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
  asg(
    io.out.bits.wPrf.wmask,
    Mux(inBits.memType === MemType.LWL, lwlWen, Mux(inBits.memType === MemType.LWR, lwrWen, "b1111".U))
  )
  // CacheInst =====================================================
  if (enableCacheInst) {
    val inci = io.in.bits.toCache2.cacheInst.get
    assert(inBits.isSQ ^ inci.valid)
    io.out.valid := Mux(inci.valid, cache2.io.cacheInst.finish.get, cacheFinish && !inBits.isSQ)
    asg(cache2.io.cacheInst.redirect.get, io.flush)
  }
}
