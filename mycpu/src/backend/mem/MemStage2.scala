package backend.mem

import bundle._
import config._
import chisel3._
import chisel3.util._
import cache._
import utils._
import utils.BytesWordUtils._
import difftest.DifftestUartBuffer
import frontend.RATWriteBackIO
import backend.mem.{MemStage1OutIO, QuerySQ}
import backend.mem.MemStage2OutIO

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
    val out     = Decoupled(new MemStage2OutIO)
    val wSrat   = Valid(new RATWriteBackIO)
    val querySQ = new QuerySQ
    val doneSQ  = Output(Bool()) //connect storeQ deq.back
    val donePC  = if (debug) Some(Output(UWord)) else None //connect storeQ deq.back
    val dmem    = new DramIO
    val flush   = Input(Bool()) // for cache instr
  })
  val outBits    = io.out.bits
  val inBits     = io.in.bits
  val prevDstSrc = word2Bytes(inBits.prevDstSrc)
  outBits.wbInfo     := inBits.wbInfo
  outBits.prevDstSrc := inBits.prevDstSrc
  outBits.exDetect   := inBits.exDetect
  // write back to RS inPrf and SRAT
  io.wSrat.bits.aDest := inBits.wbInfo.destAregAddr
  io.wSrat.bits.pDest := inBits.wbInfo.destPregAddr
  io.wSrat.valid      := io.out.fire
  when(inBits.memType === MemType.NON) {
    outBits.exDetect.happen := false.B
  }
  if (debug) asg(outBits.debugPC.get, inBits.debugPC.get)
  // ======================  Cache ============================
  val cache2  = Module(new CacheStage2(DcachRoads, DcachLineBytes, true)())
  val cinBit  = cache2.io.in.bits
  val coutBit = cache2.io.out.bits
  asg(cinBit.fromStage1, inBits.toCache2)
  asg(cinBit.ptag, inBits.pTag)
  asg(cinBit.isUncached, inBits.isUncache)
  import MemType._
  val isCi      = if (enableCacheInst) io.in.bits.toCache2.cacheInst.get.valid else false.B
  val isld      = !inBits.isSQ && isLoad(inBits.memType)
  val isNone    = inBits.memType === MemType.NON
  val cacheMask = Mux(inBits.isUncache, io.querySQ.req.needMask, io.querySQ.res.memMask)
  val inIndex   = inBits.toCache2.dCacheReq.get.lowAddr.index
  asg(cinBit.cancel, inBits.exDetect.happen)
  asg(outBits.cacheMask, cacheMask)
  // store req from rostage should not enter cache
  cache2.io.in.valid := io.in.valid && !isNone && !inBits.isWuart
  io.dmem <> cache2.io.dram
  io.in.ready := cache2.io.in.ready && io.out.ready // LSU is always priori to MDU
  val cacheFinish = cache2.io.out.valid
  // store req from rostage should not wait cache
  io.out.valid        := Mux(inBits.isSQ, false.B, Mux(isNone, true.B, cacheFinish))
  io.doneSQ           := cacheFinish && inBits.isSQ
  cache2.io.out.ready := Mux(inBits.isSQ, true.B, io.out.ready)

  if (debug) io.donePC.get := inBits.debugPC.get
  // BASIC================= Uart Buffer =======================
  val uBuffer = Module(new UartBuffer)
  uBuffer.io.enq.valid := false.B
  asg(uBuffer.io.enq.bits, cinBit.fromStage1.dCacheReq.get.wWord(7, 0))
  when(inBits.isWuart && io.in.valid) {
    io.in.ready          := uBuffer.io.enq.ready && io.out.ready
    io.doneSQ            := uBuffer.io.enq.ready
    uBuffer.io.enq.valid := inBits.isWuart
  }
  val ubaw = uBuffer.io.dram.aw
  val ubw  = uBuffer.io.dram.w
  val ubb  = uBuffer.io.dram.b
  val dcaw = cache2.io.dram.aw
  val dcw  = cache2.io.dram.w
  val dcb  = cache2.io.dram.b
  import bundle.DramWriteIO
  io.dmem.ar <> cache2.dram.ar
  io.dmem.r <> cache2.dram.r
  // AW =====================================================
  io.dmem.aw.valid := dcaw.valid || ubaw.valid
  dcaw.ready       := io.dmem.aw.ready
  ubaw.ready       := io.dmem.aw.ready && !dcaw.valid
  io.dmem.aw.bits  := Mux(dcaw.valid, dcaw.bits, ubaw.bits)
  // W ======================================================
  val currWID  = RegInit(0.U)
  val wBurstIn = RegInit(false.B)
  when(io.dmem.w.fire && !io.dmem.w.bits.last) {
    wBurstIn := true.B
    currWID  := io.dmem.w.bits.id
  }
  when(io.dmem.w.fire && io.dmem.w.bits.last) { wBurstIn := false.B }
  io.dmem.w.valid := dcw.valid || ubw.valid
  dcw.ready       := io.dmem.w.ready
  ubw.ready       := io.dmem.w.ready && !dcw.valid
  io.dmem.w.bits  := Mux(dcw.valid, dcw.bits, ubw.bits)
  when(wBurstIn) {
    dcw.ready      := Mux(isDCacheId(currWID), io.dmem.w.ready, false.B)
    ubw.ready      := Mux(isDCacheId(currWID), false.B, io.dmem.w.ready)
    io.dmem.w.bits := Mux(isDCacheId(currWID), dcw.bits, ubw.bits)
  }
  // B ======================================================
  io.dmem.b.ready := Mux(isDCacheId(io.dmem.b.bits.id), dcb.ready, ubb.ready)
  dcb.valid       := isDCacheId(io.dmem.b.bits.id) && io.dmem.b.valid
  ubb.valid       := isUartBufId(io.dmem.b.bits.id) && io.dmem.b.valid
  dcb.bits        := io.dmem.b.bits
  ubb.bits        := io.dmem.b.bits
  // ===================== select ===============================
  val lowAddr = inBits.toCache2.dCacheReq.get.lowAddr
  asg(io.querySQ.req.addr, Cat(inBits.pTag, lowAddr.index, lowAddr.offset))
  asg(io.querySQ.req.needMask, inBits.toCache2.dCacheReq.get.wStrb)
  val validWord = maskWord(coutBit.ddata.get, cacheMask).asUInt | maskWord(io.querySQ.res.data, ~cacheMask).asUInt
  asg(outBits.cacheData, coutBit.ddata.get)
  asg(outBits.storeQData, io.querySQ.res.data)
  asg(outBits.l2sb, inBits.toCache2.dCacheReq.get.lowAddr.offset(1, 0))
  asg(outBits.memType, inBits.memType)
  // CacheInst =====================================================
  if (enableCacheInst) {
    val inci = io.in.bits.toCache2.cacheInst.get
    when(io.in.valid) { assert(!inBits.isSQ || !inci.valid) }
    outBits.isCIntr := false.B
    when(inci.valid) {
      io.out.valid    := cache2.io.cacheInst.finish.get
      outBits.isCIntr := true.B
    }
    asg(cache2.io.cacheInst.redirect.get, io.flush)
  }
}

object UartBuffer {
  val totalNum = 32
  val idleMax  = 1024
  val ubId     = 3
  val burstLen = 16
  val burstWid = log2Ceil(burstLen)
  val countWid = burstWid + 1
  require(totalNum % burstLen == 0)
  class MyCount(width: Int) extends MycpuModule {
    val value   = IO(Valid(UInt(width.W)))
    val inner   = RegInit(0.U(width.W))
    val notZero = RegInit(false.B)
    value.valid := notZero
    value.bits  := inner
    def inc() = {
      when(notZero) {
        inner := inner + 1.U
      }.otherwise {
        notZero := true.B
      }
      inner.andR && notZero
    }
    def set(newValue: UInt) = {}
  }
}

class UartBuffer extends MycpuModule {
  val io = IO(new Bundle {
    val enq  = Flipped(Decoupled(UByte))
    val dram = new DramWriteIO
  })
  import UartBuffer._
  val buf = Module(new Queue(UByte, 4))
  io.enq <> buf.io.enq
  val ram = Module(new Queue(UByte, totalNum, false, false, false, false))
  buf.io.deq <> ram.io.enq
  ram.io.deq.ready := false.B
  val groupCnt = RegInit(0.U((log2Ceil(totalNum / burstLen)).W))
  val fewCnt   = RegInit(0.U((burstWid).W))
  val idleTime = Counter(idleMax)

  val fCntInc = ram.io.enq.fire
  val gCntInc = fCntInc && (fewCnt === (burstLen - 1).U)
  when(fCntInc) { fewCnt := fewCnt + 1.U }
  when(gCntInc) { groupCnt := groupCnt + 1.U }

  val idle :: wReq :: backWait :: Nil = Enum(3)

  val awOk = RegInit(false.B)
  val wOk  = RegInit(false.B)

  val state    = RegInit(idle)
  val burstCnt = Reg(UInt(burstWid.W))
  val burstReg = Reg(UInt(burstWid.W))
  val byteBuf  = RegEnable(ram.io.deq.bits, ram.io.deq.fire)

  def wReqInit(burstNum: UInt) = {
    state            := wReq
    awOk             := false.B
    wOk              := false.B
    ram.io.deq.ready := true.B
    burstReg         := burstNum
    burstCnt         := burstNum
  }
  val aw = io.dram.aw
  val w  = io.dram.w
  val b  = io.dram.b
  asg(aw.valid, false.B)
  asg(aw.bits.addr, "h1faf_fff0".U(32.W))
  asg(aw.bits.burst, BurstType.FIXED)
  asg(aw.bits.id, ubId.U(4.W))
  asg(aw.bits.len, burstReg)
  asg(aw.bits.size, SizeType.Byte.asUInt)
  asg(w.valid, false.B)
  asg(w.bits.data, Cat(0.U(24.W), byteBuf))
  asg(w.bits.id, ubId.U(4.W))
  asg(w.bits.strb, "b0001".U(4.W))
  asg(w.bits.last, false.B)
  asg(b.ready, false.B)
  val diffOutFire = WireInit(false.B)
  val diffOutNum  = WireInit(0.U)

  switch(state) {
    is(idle) {
      when(groupCnt =/= 0.U) {
        diffOutFire := true.B
        diffOutNum  := 15.U
        wReqInit((burstLen - 1).U)
        asg(groupCnt, groupCnt - 1.U + gCntInc.asUInt)
      }.elsewhen(idleTime.inc() && ram.io.deq.valid) {
        assert(fewCnt =/= 0.U)
        diffOutFire := true.B
        val tmp = fewCnt - 1.U
        diffOutNum := tmp
        wReqInit(tmp)
        asg(fewCnt, ZeroExt(fCntInc.asUInt, 4))
      }
    }
    is(wReq) {
      aw.valid := !awOk
      w.valid  := !wOk
      when(aw.fire) { awOk := true.B }
      when(w.fire) {
        asg(wOk, burstCnt === 0.U)
        asg(w.bits.last, burstCnt === 0.U)
        asg(ram.io.deq.ready, burstCnt =/= 0.U)
        asg(burstCnt, burstCnt - 1.U)
      }
      when(awOk && wOk) { state := backWait }
    }
    is(backWait) {
      b.ready := true.B
      when(b.fire) {
        when(groupCnt =/= 0.U) {
          wReqInit(burstLen.U)
          asg(groupCnt, groupCnt - 1.U + gCntInc.asUInt)
        }.otherwise {
          state := idle
          idleTime.reset()
        }
      }
    }
  }
  if (verilator) {
    val diffUbuffer = Module(new DifftestUartBuffer)
    diffUbuffer.io.clock    := clock
    diffUbuffer.io.en       := !reset.asBool
    diffUbuffer.io.awFire   := diffOutFire
    diffUbuffer.io.awLen    := diffOutNum
    diffUbuffer.io.enqFire  := ram.io.enq.fire
    diffUbuffer.io.curGroup := groupCnt
    diffUbuffer.io.curFew   := fewCnt
    diffUbuffer.io.state    := state
    diffUbuffer.io.deqValid := ram.io.deq.valid
    diffUbuffer.io.wFire    := io.dram.w.fire
    diffUbuffer.io.wChar    := io.dram.w.bits.data
    diffUbuffer.io.enqChar  := ram.io.enq.bits
  }
}
