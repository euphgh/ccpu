package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg
import utils.PipelineConnect
import chisel3.util.experimental.BoringUtils._
import utils.PriorityCount
import utils.SignExt
import config.MycpuInit.PCReset
import difftest._
import utils._

/**
  * preif.in.redirect:
  *     io.redirect:
  *         <exe>:mispreRedirect
  *         <CP0>:exception/eret redirect
  *     if2.redirect:
  *         noBrMispreRedirect
  * if1.in.bpuUpdata:
  *     io.bpuUpdate:
  *         <exe>:use real branch info to update
  *         btb:type and target
  *         pht:take
  *     if2.bpuUpdate:
  *         btb entry type
  *         pht entry b00
  */
class InstFetch extends MycpuModule {
  val io = IO(new Bundle {

    val redirect = Flipped(new FrontRedirctIO)
    val out      = Decoupled(new IfStage2OutIO)

    val tlb         = new TLBSearchIO
    val imem        = new DramReadIO
    val bpuUpdateIn = Flipped(new BpuUpdateIO)
  })

  val preIfStage = Module(new PreIf)
  val ifStage1   = Module(new IfStage1)
  val ifStage2   = Module(new IfStage2)
  val dsGoIf2    = WireInit(ifStage1.io.isDelaySlot && ifStage1.io.out.fire)

  asg(
    preIfStage.io.in.redirect.target,
    Mux(io.redirect.flush, io.redirect.target, ifStage2.io.out.bits.dsDstRedir.target)
  )
  asg(
    preIfStage.io.in.redirect.flush,
    io.redirect.flush || ifStage2.io.out.bits.dsDstRedir.flush || dsGoIf2
  )
  asg(preIfStage.io.in.fromIf1, ifStage1.io.toPreIf)
  asg(preIfStage.io.in.isDSredir, ifStage2.io.out.bits.isDSredir && !io.redirect.flush)

  //If1 in
  asg(ifStage1.io.in, preIfStage.io.out)
  ifStage1.io.tlb <> io.tlb
  val stage1IsCacheInstr = ifStage1.io.out.bits.iCache.cacheInst.get.valid

  val iCacheInst = Wire(Flipped(Valid(new ICacheInstIO)))
  if (enableCacheInst) {
    addSink(iCacheInst, "ICacheInstrReq")
  } else {
    iCacheInst.bits  := DontCare
    iCacheInst.valid := false.B
  }
  //IF2 in
  PipelineConnect(
    ifStage1.io.out,
    ifStage2.io.in,
    ifStage2.io.out.fire,
    !stage1IsCacheInstr && (io.redirect.flush || iCacheInst.valid || ifStage2.io.selfFlush)
  )
  ifStage2.io.backFlush := io.redirect.flush
  ifStage2.io.dsGoIf2   := dsGoIf2
  ifStage2.io.imem <> io.imem
  io.out <> ifStage2.io.out

  // ifStage2 update BPU
  class BtbAssignBundle extends MycpuBundle {
    val tagIdx   = UInt((32 - log2Ceil(IcachLineBytes)).W)
    val instrOff = Vec(fetchNum, UInt(instrOffWidth.W))
    val wen      = Vec(fetchNum, Bool())
    val data     = Vec(fetchNum, new BtbOutIO)
    def passToUpdateIO(updateIO: BtbUpdateIO) = {
      asg(updateIO.tagIdx, tagIdx)
      asg(updateIO.instrOff, instrOff)
      (0 until fetchNum).foreach(i => {
        asg(updateIO.data(i).valid, wen(i))
        asg(updateIO.data(i).bits, data(i))
      })
    }
  }

  val if2Wio       = ifStage2.io.btbDeq.bits
  val if2AssignBtb = Wire(new BtbAssignBundle)
  asg(if2AssignBtb.tagIdx, if2Wio.tagIdx)
  val if2OutWen      = Wire(Vec(fetchNum, Bool()))
  val if2OutTarget   = Wire(Vec(fetchNum, UWord))
  val if2OutInstType = Wire(Vec(fetchNum, BtbType()))
  (0 until fetchNum).foreach(i => {
    import BranchType._
    val pc = Cat(if2Wio.tagIdx, if2Wio.instrOff(i), 0.U(2.W))
    require(pc.getWidth == 32)
    asg(if2OutWen(i), ifStage2.io.btbDeq.valid && if2Wio.valid(i) && (isB(if2Wio.brType(i)) || isJ(if2Wio.brType(i))))
    asg(if2OutTarget(i), getDst(if2Wio.brType(i), if2Wio.instr(i), pc))
    asg(if2OutInstType(i), toBtbType(if2Wio.brType(i), if2Wio.instr(i)(25, 21)))
  })

  (0 until fetchNum).map(i => {
    val sel = VecInit((0 until fetchNum).map(j => { if2Wio.instrOff(j)(1, 0) === i.U }))
    when(ifStage2.io.btbDeq.valid) { PopCount(sel.asUInt === 1.U) }
    asg(if2AssignBtb.instrOff(i), Mux1H(sel, if2Wio.instrOff))
    asg(if2AssignBtb.wen(i), Mux1H(sel, if2OutWen))
    asg(if2AssignBtb.data(i).target, Mux1H(sel, if2OutTarget))
    asg(if2AssignBtb.data(i).instType, Mux1H(sel, if2OutInstType))
  })

  val if2NoBr  = if2OutWen.asUInt.orR === false.B
  val backWbtb = io.bpuUpdateIn.btb.valid
  asg(ifStage2.io.btbDeq.ready, if2NoBr || !backWbtb)

  // backend update
  val backAssignBtb = Wire(new BtbAssignBundle)
  val if2PhtIO      = Wire(new PhtUpdateIO)
  val if2BtbIO      = Wire(new BtbUpdateIO)
  val tagIdx        = io.bpuUpdateIn.pc(31, instrOffMsb + 1)
  val instrOff      = io.bpuUpdateIn.pc(instrOffMsb, instrOffLsb)
  val selValid      = VecInit.tabulate(fetchNum)(i => instrOff(1, 0) === i.U)
  asg(backAssignBtb.tagIdx, tagIdx)
  asg(backAssignBtb.instrOff, VecInit.fill(fetchNum)(instrOff))
  (0 until fetchNum).foreach(i => {
    asg(backAssignBtb.wen(i), selValid(i) && io.bpuUpdateIn.btb.valid)
    asg(backAssignBtb.data(i), io.bpuUpdateIn.btb.bits)
  })
  asg(if2PhtIO.tagIdx, tagIdx)
  asg(if2PhtIO.instrOff, VecInit.fill(fetchNum)(instrOff))
  (0 until fetchNum).foreach(i => {
    if2PhtIO.data(i).valid := selValid(i) && io.bpuUpdateIn.pht.valid
    if2PhtIO.data(i).bits  := io.bpuUpdateIn.pht.bits
  })
  if2AssignBtb.passToUpdateIO(if2BtbIO)
  when(backWbtb) { backAssignBtb.passToUpdateIO(if2BtbIO) }
  (0 until (fetchNum)).map(i => asg(io.out.bits.isBd(i), false.B))

  // >> bpu ===============================================
  val ras = Module(new RetAddrStack(true, retAddrStackSize))
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  val lht = Module(new LocHisTab())

  val bpuGoNext = ifStage1.io.out.fire
  btb.goNext := bpuGoNext
  pht.goNext := bpuGoNext
  lht.goNext := bpuGoNext
  asg(btb.update.tagIdx, if2BtbIO.tagIdx)
  asg(btb.update.instrOff, if2BtbIO.instrOff)
  asg(btb.update.data, if2BtbIO.data)
  asg(pht.update.tagIdx, if2PhtIO.tagIdx)
  asg(pht.update.instrOff, if2PhtIO.instrOff)
  asg(lht.update.tagIdx, if2PhtIO.tagIdx)
  asg(lht.update.instrOff, if2PhtIO.instrOff)
  ras.io.push <> ifStage2.io.rasPush
  ras.io.pop := ifStage2.io.rasPop
  (0 until fetchNum).map(i => {
    asg(pht.update.data(i).valid, if2PhtIO.data(i).valid)
    asg(pht.update.data(i).bits, if2PhtIO.data(i).bits.cnt)
    asg(lht.update.data(i).valid, if2PhtIO.data(i).valid)
    asg(lht.update.data(i).bits, if2PhtIO.data(i).bits.take)
  })
  if (verilator) {
    val btbDiff = Module(new DifftestBTBWrite)
    asg(btbDiff.io.clock, clock)
    asg(btbDiff.io.en, btbDiff.io.wen.asUInt.orR)
    asg(btbDiff.io.tagIdx, if2BtbIO.tagIdx)
    asg(btbDiff.io.instrOff, if2BtbIO.instrOff)
    asg(btbDiff.io.wen, VecInit(if2BtbIO.data.map(_.valid)))
    asg(btbDiff.io.target, VecInit(if2BtbIO.data.map(_.bits.target)))
    asg(btbDiff.io.btbType, VecInit(if2BtbIO.data.map(_.bits.instType.asUInt)))

    val phtDiff = Module(new DifftestPHTWrite)
    asg(phtDiff.io.clock, clock)
    asg(phtDiff.io.en, phtDiff.io.wen.asUInt.orR)
    asg(phtDiff.io.tagIdx, if2PhtIO.tagIdx)
    asg(phtDiff.io.instrOff, if2PhtIO.instrOff)
    asg(phtDiff.io.wen, VecInit(if2PhtIO.data.map(_.valid)))
    asg(phtDiff.io.count, VecInit(if2PhtIO.data.map(_.bits.cnt)))
    asg(phtDiff.io.take, VecInit(if2PhtIO.data.map(_.bits.take)))
  }
  val npc     = preIfStage.io.out.npc
  val bpuRreq = ifStage1.io.bpuRreq
  (0 until fetchNum).foreach(i => {
    val offMsb = log2Ceil(IcachLineBytes / 4) + 2
    val mid    = Wire(UInt((offMsb - 2).W))
    asg(mid, RingBits(npc(offMsb - 1, 2), fetchNum, i))
    val searchAddr = Cat(npc(31, offMsb), mid, 0.U(2.W))
    asg(btb.readAddr(i).bits, searchAddr)
    asg(pht.readAddr(i).bits, searchAddr)
    asg(lht.readAddr(i).bits, searchAddr)
    asg(btb.readAddr(i).valid, bpuRreq)
    asg(pht.readAddr(i).valid, bpuRreq)
    asg(lht.readAddr(i).valid, bpuRreq)
    asg(ifStage2.io.btbRes(i), btb.readRes(i))
    asg(ifStage2.io.phtRes(i), pht.readRes(i))
    asg(ifStage2.io.lhtRes(i), lht.readRes(i))
  })
  asg(ifStage2.io.rasTop, RegEnable(ras.io.topData, bpuRreq))
}
