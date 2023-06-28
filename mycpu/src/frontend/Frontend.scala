package frontend

import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg
import utils.PipelineConnect

class Frontend extends MycpuModule {
  val io = IO(new Bundle {
    val redirect = Flipped(new FrontRedirctIO)
    val out      = Vec(decodeNum, Decoupled(new InstBufferOutIO))

    val tlb         = new TLBSearchIO
    val imem        = new DramReadIO
    val bpuUpdateIn = Flipped(Valid(new BpuUpdateIO))
    val icacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new ICacheInstIO)))
      else None
  })

  val instFetch  = Module(new InstFetch)
  val instBuffer = Module(new InstBuffer)

  asg(instFetch.io.bpuUpdateIn, io.bpuUpdateIn)
  asg(instFetch.io.imem, io.imem)
  asg(instFetch.io.redirect, io.redirect)
  asg(instFetch.io.tlb, io.tlb)
  if (enableCacheInst) { asg(instFetch.io.icacheInst.get, io.icacheInst.get) }

  instFetch.io.out <> instBuffer.io.in //not pipeline connect
  io.out <> instBuffer.io.out
}
