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

    val tlbSearch   = new TLBSearchIO
    val imem        = new DramReadIO
    val bpuUpdateIn = Flipped(Valid(new BpuUpdateIO))
  })

  val instFetch  = Module(new InstFetch)
  val instBuffer = Module(new InstBuffer)

  asg(instFetch.io.bpuUpdateIn, io.bpuUpdateIn)
  instFetch.io.imem <> io.imem
  asg(instFetch.io.redirect, io.redirect)
  instFetch.io.tlb <> io.tlbSearch

  instFetch.io.out <> instBuffer.io.in //not pipeline connect
  io.out <> instBuffer.io.out
  asg(instBuffer.io.flush, io.redirect.flush)
}
