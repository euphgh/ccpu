package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg
import utils.PipelineConnect
import chisel3.util.experimental.BoringUtils
import utils.PriorityCount
import utils.SignExt
import config.MycpuInit.PCReset

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

  asg(preIfStage.io.in.redirect, Mux(io.redirect.flush, io.redirect, ifStage2.io.noBrMispreRedirect))
  asg(preIfStage.io.in.fromIf1, ifStage1.io.toPreIf)

  //If1 in
  asg(ifStage1.io.in, preIfStage.io.out)
  ifStage1.io.tlb <> io.tlb
  val stage1IsCacheInstr = ifStage1.io.out.bits.iCache.cacheInst.get.valid

  //IF2 in
  PipelineConnect(
    ifStage1.io.out,
    ifStage2.io.in,
    ifStage2.io.out.fire,
    // noBrMissFlush only set when if2 out fire
    !stage1IsCacheInstr && (io.redirect.flush || ifStage2.io.noBrMispreRedirect.flush)
  )
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
    dontTouch(pc)
    require(pc.getWidth == 32)
    asg(if2OutWen(i), ifStage2.io.btbDeq.valid && if2Wio.valid(i) && (isB(if2Wio.brType(i)) || isJ(if2Wio.brType(i))))
    asg(if2OutTarget(i), getDst(if2Wio.brType(i), if2Wio.instr(i), pc))
    asg(if2OutInstType(i), toBtbType(if2Wio.brType(i), if2Wio.instr(i)(25, 21)))
  })

  (0 until fetchNum).map(i => {
    val sel = VecInit((0 until fetchNum).map(j => { if2Wio.instrOff(j)(1, 0) === i.U }))
    dontTouch(sel)
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
  val if2PhtIO      = ifStage1.io.phtUpdate
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
  if2AssignBtb.passToUpdateIO(ifStage1.io.btbUpdate)
  when(backWbtb) { backAssignBtb.passToUpdateIO(ifStage1.io.btbUpdate) }

  /**
    * isBd:
    *   handle in Instfetch
    *   need flush signal
    */
  val outBits = io.out.bits
  val validBr = WireInit(
    VecInit(
      (0 until fetchNum).map(i => BranchType.isBr(outBits.realBrType(i)) && outBits.validMask(i) && io.out.valid)
    )
  )
  val dsReg       = RegInit(false.B)
  val lastDs      = WireInit(0.U((1 + log2Up(fetchNum)).W))
  val outValidNum = PriorityCount(outBits.validMask.asUInt & SignExt(io.out.valid, fetchNum))
  when(outBits.validMask(0) && io.out.fire) { dsReg := false.B }
  when(validBr.asUInt.orR) {
    asg(lastDs, fetchNum.U - PriorityEncoder(validBr.reverse)) //最后一个延迟槽对应取过来的四条指令的哪一个
    when(lastDs >= outValidNum && io.out.fire) {
      dsReg := true.B
    }
  }
  val isBd = outBits.isBd
  (0 until (fetchNum - 1)).map(i => asg(isBd(i + 1), validBr(i)))
  asg(isBd(0), dsReg)
  when(io.redirect.flush || ifStage2.io.noBrMispreRedirect.flush) { dsReg := false.B }
}
