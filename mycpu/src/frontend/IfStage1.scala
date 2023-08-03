package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._

import cache._
import utils._
import chisel3.util.experimental.decode._
import chisel3.util.experimental.BoringUtils._
import config.MycpuInit.PCReset

class ICacheInstIO extends MycpuBundle {
  val op    = CacheOp()
  val taglo = UWord
  val index = UInt(IcacheIndexWidth.W)
}

/**
  * not connect by pipeline
  * out.pcVal = regEnable(preIFOutIO.npc, fire)
  *
  * out.bpuOut give predict result after fire posedge
  * should keep result until next fire posedge
  *
  * out.alignMask is calculate by out.pcVal
  *
  * out.tagOfInstGroup is calculate by out.pcVal
  * connect TLB module by tlb bundle
  * tlb.req = out.pcVal
  * out.tagOfInstGroup  = tlb.back
  *
  * out.exception != NONE when tlb exception or address error
  *
  * instantiate I-cache stage1 in this module
  * all out should not change until next fire posedge
  * when in.flush || IcacheInst.valid = true
  * stage1 must update all out in next cycle
  *
  * instantiate BPU in this module, let out.bpuout = bpu.out
  */
class IfStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in          = Flipped(new PreIfOutIO)
    val out         = Decoupled(new IfStage1OutIO)
    val bpuRreq     = Output(Bool())
    val toPreIf     = new IfStage1ToPreIf
    val tlb         = new TLBSearchIO
    val isDelaySlot = Output(Bool())
    val bCacheW     = Flipped(Valid(new BCache.BCacheWIO))
  })

  val icacheInst =
    if (enableCacheInst) Some(Wire(Flipped(Valid(new ICacheInstIO))))
    else None
  if (enableCacheInst) {
    addSink(icacheInst.get, "ICacheInstrReq")
  }
  // alias ===============================================
  val npc = io.in.npc

  // stage regs ==========================================
  val fakeCacheInst = Wire(Flipped(Valid(new ICacheInstIO)))
  fakeCacheInst.valid := false.B
  fakeCacheInst.bits  := 0.U.asTypeOf(new ICacheInstIO)
  val usableCacheInst = icacheInst.getOrElse(fakeCacheInst)
  val isCacheInst     = usableCacheInst.valid
  val update          = WireInit(io.in.flush || isCacheInst || io.out.ready)
  io.bpuRreq := io.in.flush || io.out.ready
  val pc     = RegEnable(npc, PCReset, update)
  val iciTag = RegEnable(usableCacheInst.bits.taglo, usableCacheInst.valid)
  io.out.bits.bpuSel := VecInit.tabulate(fetchNum)(i => RegEnable(npc(3, 2) + i.U, PCReset(3, 2), update))
  val isDelaySlot = RegEnable(io.in.isDelaySlot, false.B, update)
  asg(io.isDelaySlot, isDelaySlot)
  // BPU Cache ===========================================
  val bCache = Module(new BCache)
  bCache.io.write <> io.bCacheW
  asg(bCache.io.readAddr.bits, npc)
  asg(bCache.io.readAddr.valid, io.bpuRreq)
  asg(io.toPreIf.predictRes, bCache.io.readRes)
  asg(io.out.bits.bCacheDst, bCache.io.readRes)
  // use wire io.in direct ================================
  // >> cache =============================================
  val icache1 = Module(new CacheStage1(IcachRoads, IcachLineBytes, false))
  icache1.io.in.valid                         := update
  icache1.io.in.bits.ifReq.get.index          := Mux(isCacheInst, usableCacheInst.bits.index, getAddrIdxI(npc))
  icache1.io.in.bits.ifReq.get.offset         := getOffsetI(npc)
  icache1.io.in.bits.cacheInst.get.valid      := isCacheInst
  icache1.io.in.bits.cacheInst.get.bits.op    := usableCacheInst.bits.op
  icache1.io.in.bits.cacheInst.get.bits.taglo := usableCacheInst.bits.taglo
  io.out.bits.iCache <> icache1.io.out
  // Mask and Dest ===============================
  io.out.bits.alMask := Mux(
    isDelaySlot,
    "b0001".U,
    decoder(
      pc(instrOffMsb, 2),
      TruthTable(
        Seq(
          BitPat("b" + "1" * (instrOffWidth - 2) + "0" + "1") -> BitPat("b0111"),
          BitPat("b" + "1" * (instrOffWidth - 2) + "1" + "0") -> BitPat("b0011"),
          BitPat("b" + "1" * (instrOffWidth - 2) + "1" + "1") -> BitPat("b0001")
        ),
        BitPat("b1111")
      )
    )
  )
  // >> output ================
  val addrError = pc(1, 0).orR
  io.toPreIf.pcVal  := pc
  io.out.bits.pcVal := pc
  // >> tlb ================
  val tlbSearchTick = RegNext(update, true.B)
  val tlbRes        = HoldUnless(io.tlb.res, tlbSearchTick)
  val tlbExp        = tlbRes.refill || !tlbRes.hit
  io.tlb.req.bits            := pc
  io.tlb.req.valid           := tlbSearchTick
  io.out.bits.tagOfInstGroup := tlbRes.pTag
  io.out.bits.exception := MuxCase(
    FrontExcCode.NONE,
    Seq(
      addrError -> FrontExcCode.AdEL,
      tlbExp -> Mux(
        tlbRes.refill,
        FrontExcCode.RefillTLBL,
        FrontExcCode.InvalidTLBL
      )
    )
  )
  io.out.bits.isUncached := tlbRes.ccAttr =/= CCAttr.Cached
  if (enableCacheInst) {
    // index type cache instr should not require tlb
    // becasue, way infomation is in tag
    val ci = icache1.io.out.cacheInst.get
    when(ci.valid && CacheOp.isIdxInv(ci.bits.op)) {
      io.tlb.req.valid := false.B
    }
    when(ci.valid && !io.out.fire) {
      update := false.B
    }
    when(ci.valid) {
      io.tlb.req.bits := iciTag
    }
  }
  io.out.valid := true.B

  // Reset for 512
  val resetWidth = log2Ceil(1024)
  val resetCnt   = RegInit(0.U(resetWidth.W))
  when(resetCnt =/= ~(0.U(resetWidth.W))) {
    resetCnt := resetCnt + 1.U
    asg(io.out.valid, false.B)
    asg(update, false.B)
  }
}
