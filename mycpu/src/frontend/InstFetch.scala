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
    val bpuUpdateIn = Flipped(Valid(new BpuUpdateIO))
  })

  val preIfStage = Module(new PreIf)
  val ifStage1   = Module(new IfStage1)
  val ifStage2   = Module(new IfStage2)

  //PreIf in
  if (hasSnapShot) {
    val resetRedirect = Wire(new FrontRedirctIO)
    resetRedirect.flush  := true.B
    resetRedirect.target := PCReset
    asg(
      preIfStage.io.in.redirect,
      Mux(RegNext(reset.asBool), resetRedirect, Mux(io.redirect.flush, io.redirect, ifStage2.io.noBrMispreRedirect))
    )
  } else {
    asg(preIfStage.io.in.redirect, Mux(io.redirect.flush, io.redirect, ifStage2.io.noBrMispreRedirect))
  }
  asg(preIfStage.io.in.fromIf1, ifStage1.io.toPreIf)

  //If1 in
  asg(ifStage1.io.in, preIfStage.io.out)
  ifStage1.io.tlb <> io.tlb
  asg(ifStage1.io.bpuUpdateIn, Mux(ifStage2.io.bpuUpdate.fire, ifStage2.io.bpuUpdate.bits, io.bpuUpdateIn.bits))
  when(!io.bpuUpdateIn.valid && !ifStage2.io.bpuUpdate.valid) {
    asg(ifStage1.io.bpuUpdateIn.btb.valid, false.B)
    asg(ifStage1.io.bpuUpdateIn.pht.valid, false.B)
  }
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
  asg(ifStage2.io.bpuUpdate.ready, !io.bpuUpdateIn.valid)

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
