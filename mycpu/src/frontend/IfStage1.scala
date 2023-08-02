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
import difftest._

class ICacheInstIO extends MycpuBundle {
  val op    = CacheOp()
  val taglo = UWord
  val index = UInt(cacheIndexWidth.W)
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
    val in      = Flipped(new PreIfOutIO)
    val out     = Decoupled(new IfStage1OutIO)
    val toPreIf = new IfStage1ToPreIf
    val tlb     = new TLBSearchIO

    val isDelaySlot = Output(Bool())

    val btbUpdate = new BtbUpdateIO
    val phtUpdate = new PhtUpdateIO
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
  val pc              = RegEnable(npc, PCReset, update)
  val iciTag          = RegEnable(usableCacheInst.bits.taglo, usableCacheInst.valid)
  val bpuSel          = VecInit.tabulate(fetchNum)(i => RegEnable(npc(3, 2) + i.U, PCReset(3, 2), update))
  val isDelaySlot     = RegEnable(io.in.isDelaySlot, false.B, update)
  asg(io.isDelaySlot, isDelaySlot)
  // use wire io.in direct ================================
  // >> cache =============================================
  val icache1 = Module(new CacheStage1())
  icache1.io.in.valid                         := update
  icache1.io.in.bits.ifReq.get.index          := Mux(isCacheInst, usableCacheInst.bits.index, getAddrIdx(npc))
  icache1.io.in.bits.ifReq.get.offset         := getOffset(npc)
  icache1.io.in.bits.cacheInst.get.valid      := isCacheInst
  icache1.io.in.bits.cacheInst.get.bits.op    := usableCacheInst.bits.op
  icache1.io.in.bits.cacheInst.get.bits.taglo := usableCacheInst.bits.taglo
  io.out.bits.iCache <> icache1.io.out
  // >> bpu ===============================================
  val PCs   = (0 until fetchNum).map(i => Cat(npc(31, 4), (npc(3, 2) + i.U), "b00".U))
  val bpuPC = (0 until fetchNum).map(i => Cat(npc(31, 4), i.U(log2Ceil(fetchNum).W), "b00".U))
  // >> >> module ============================================
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  val lht = Module(new LocHisTab())
  val ras = Module(new RetAddrStack(true, retAddrStackSize))
  // >> >> >> write =======================================
  asg(btb.update.tagIdx, io.btbUpdate.tagIdx)
  asg(btb.update.instrOff, io.btbUpdate.instrOff)
  asg(btb.update.data, io.btbUpdate.data)
  asg(pht.update.tagIdx, io.phtUpdate.tagIdx)
  asg(pht.update.instrOff, io.phtUpdate.instrOff)
  asg(lht.update.tagIdx, io.phtUpdate.tagIdx)
  asg(lht.update.instrOff, io.phtUpdate.instrOff)
  (0 until fetchNum).map(i => {
    asg(pht.update.data(i).valid, io.phtUpdate.data(i).valid)
    asg(pht.update.data(i).bits, io.phtUpdate.data(i).bits.cnt)
    asg(lht.update.data(i).valid, io.phtUpdate.data(i).valid)
    asg(lht.update.data(i).bits, io.phtUpdate.data(i).bits.take)
  })
  if (verilator) {
    val btbDiff = Module(new DifftestBTBWrite)
    asg(btbDiff.io.clock, clock)
    asg(btbDiff.io.en, btbDiff.io.wen.asUInt.orR)
    asg(btbDiff.io.tagIdx, io.btbUpdate.tagIdx)
    asg(btbDiff.io.instrOff, io.btbUpdate.instrOff)
    asg(btbDiff.io.wen, VecInit(io.btbUpdate.data.map(_.valid)))
    asg(btbDiff.io.target, VecInit(io.btbUpdate.data.map(_.bits.target)))
    asg(btbDiff.io.btbType, VecInit(io.btbUpdate.data.map(_.bits.instType.asUInt)))

    val phtDiff = Module(new DifftestPHTWrite)
    asg(phtDiff.io.clock, clock)
    asg(phtDiff.io.en, phtDiff.io.wen.asUInt.orR)
    asg(phtDiff.io.tagIdx, io.phtUpdate.tagIdx)
    asg(phtDiff.io.instrOff, io.phtUpdate.instrOff)
    asg(phtDiff.io.wen, VecInit(io.phtUpdate.data.map(_.valid)))
    asg(phtDiff.io.count, VecInit(io.phtUpdate.data.map(_.bits.cnt)))
    asg(phtDiff.io.take, VecInit(io.phtUpdate.data.map(_.bits.take)))
  }
  // >> >> >> read =========================================
  val bpuout = Wire(Vec(fetchNum, new PredictResultBundle))
  val lhtout = Wire(Vec(fetchNum, new LocHisTab.LhtOutIO))
  val btbRes = Wire(Vec(fetchNum, new BtbOutIO))
  val phtRes = Wire(Vec(fetchNum, UInt(2.W)))
  val lhtRes = Wire(Vec(fetchNum, new LocHisTab.LhtOutIO))
  (0 until fetchNum).foreach(i => {
    val offMsb = log2Ceil(IcachLineBytes / 4) + 2
    val mid    = Wire(UInt((offMsb - 2).W))
    asg(mid, RingBits(npc(offMsb - 1, 2), fetchNum, i))
    val searchAddr = Cat(npc(31, offMsb), mid, 0.U(2.W))
    asg(btb.readAddr(i).bits, searchAddr)
    asg(pht.readAddr(i).bits, searchAddr)
    asg(lht.readAddr(i).bits, searchAddr)
    asg(btb.readAddr(i).valid, update)
    asg(pht.readAddr(i).valid, update)
    asg(lht.readAddr(i).valid, update)
    asg(btbRes(i), btb.readRes(i))
    asg(phtRes(i), pht.readRes(i))
    asg(lhtRes(i), lht.readRes(i))
  })
  (0 until fetchNum).foreach(i => {
    bpuout(i).btbType := btbRes(bpuSel(i)).instType
    bpuout(i).target  := Mux(bpuout(i).btbType =/= BtbType.jret, btbRes(bpuSel(i)).target, ras.io.topData)
    bpuout(i).counter := phtRes(bpuSel(i))
    lhtout(i)         := lhtRes(bpuSel(i))
  })
  io.out.bits.predictResult := bpuout
  // >> >> >> Mask and Dest ===============================
  val inst4to2 = pc(4, 2)
  val alignMask = Mux(
    isDelaySlot,
    "b0001".U,
    decoder(
      inst4to2,
      TruthTable(
        Seq(
          BitPat("b0??") -> BitPat("b1111"),
          BitPat("b100") -> BitPat("b1111"),
          BitPat("b101") -> BitPat("b0111"),
          BitPat("b110") -> BitPat("b0011"),
          BitPat("b111") -> BitPat("b0001")
        ),
        BitPat("b1111")
      )
    )
  )
  val validBranch = WireInit(VecInit.fill(fetchNum)(false.B))
  val takeMask    = Wire(Vec(fetchNum, Bool()))
  val dsMask      = Wire(UInt(fetchNum.W)) // the validMask when branch and it's ds are valid
  (0 until fetchNum).foreach(i => {
    val brIsTake =
      Mux(
        lhtout(i).cnt < 14.U,
        bpuout(i).counter > 1.U,
        lhtout(i).take
      )
    val isTakeBr = brIsTake // && bpuout(i).btbType === BtbType.b
    val isTakeJp = BtbType.isJump(bpuout(i).btbType)
    takeMask(i)     := isTakeJp || isTakeBr
    validBranch(i)  := takeMask(i) && alignMask(i)
    bpuout(i).taken := takeMask(i)
  })

  def getByVB[T <: Data](a: Seq[T]) = {
    val res = PriorityMux(validBranch.zip(a))
    res
  }
  val predDst = getByVB(bpuout.map(_.target))
  asg(io.toPreIf.predictDst, predDst)
  asg(io.out.bits.toPreIfDst, predDst)
  io.out.bits.dsFetch := !getByVB((1 until fetchNum).map(alignMask(_)) :+ false.B) && io.toPreIf.hasBranch
  dsMask              := getByVB(Seq("b0011".U(4.W), "b0111".U(4.W), "b1111".U(4.W), "b1111".U(4.W)))
  asg(io.out.bits.firstPredTake, VecInit(PriorityEncoderOH(validBranch)))
  asg(io.toPreIf.hasBranch, validBranch.asUInt.orR) // make sure Priority can not be zero
  asg(io.out.bits.dsMask, dsMask)
  asg(io.out.bits.alMask, alignMask)
  asg(io.out.bits.hasBr, io.toPreIf.hasBranch)
  // >> >> >> Update RAS ==================================
  val firValidBtbType = getByVB(bpuout.map(_.btbType))
  ras.io.push.valid := firValidBtbType === BtbType.jcall && io.out.fire && io.toPreIf.hasBranch
  ras.io.pop        := firValidBtbType === BtbType.jret && io.out.fire && io.toPreIf.hasBranch
  asg(ras.io.push.bits, getByVB((0 until fetchNum).map(i => Cat((pc(31, 2) + i.U + 2.U), pc(1, 0)))))
  // use regs in, only combinatorial logic ================
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
