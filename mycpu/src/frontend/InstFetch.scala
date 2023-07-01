package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg
import utils.PipelineConnect

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
    val icacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new ICacheInstIO)))
      else None
  })

  val preIfStage = Module(new PreIf)
  val ifStage1   = Module(new IfStage1)
  val ifStage2   = Module(new IfStage2)

  //PreIf in
  asg(preIfStage.io.in.redirect, Mux(io.redirect.flush, io.redirect, ifStage2.io.noBrMispreRedirect))
  asg(preIfStage.io.in.fromIf1, ifStage1.io.toPreIf)

  //If1 in
  asg(ifStage1.io.in, preIfStage.io.out)
  ifStage1.io.tlb <> io.tlb
  if (enableCacheInst) { asg(ifStage1.io.icacheInst.get, io.icacheInst.get) }
  asg(ifStage1.io.bpuUpdateIn, Mux(ifStage2.io.bpuUpdate.fire, ifStage2.io.bpuUpdate.bits, io.bpuUpdateIn.bits))
  when(!io.bpuUpdateIn.valid && !ifStage2.io.bpuUpdate.valid) {
    asg(ifStage1.io.bpuUpdateIn.btb.valid, false.B)
    asg(ifStage1.io.bpuUpdateIn.pht.valid, false.B)
  }

  //IF2 in
  PipelineConnect(ifStage1.io.out, ifStage2.io.in, ifStage2.io.out.fire, preIfStage.io.in.redirect.flush)
  ifStage2.io.imem <> io.imem
  io.out <> ifStage2.io.out
  asg(ifStage2.io.bpuUpdate.ready, !io.bpuUpdateIn.valid)
  asg(ifStage2.io.flushIn, io.redirect.flush)
}
