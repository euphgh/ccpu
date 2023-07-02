import chisel3._
import chisel3.util._
import config._
import frontend._
import backend._
import utils._
import bundle._

class CCPU extends MycpuModule {
  val io = IO(new Bundle {
    val extInt = Input(UInt(6.W))
    val axi    = new AxiIO
  })
  val frontend = Module(new Frontend)
  val backend  = Module(new Backend)
  val fIO      = frontend.io
  val bIO      = backend.io
  val flush    = bIO.redirectFront.flush

  asg(fIO.bpuUpdateIn, bIO.bpuUpdate)
  asg(fIO.redirect, bIO.redirectFront)
  asg(bIO.extInt, io.extInt)
  bIO.fronTlbSearch <> fIO.tlbSearch
  IbfConnectDper(new InstBufferOutIO, fIO.out, bIO.in, bIO.dperOutFireNum, flush)

  val axi2to1Arbiter = Module(new Axi2to1Arbiter)
  axi2to1Arbiter.io.dmem <> backend.io.dram
  axi2to1Arbiter.io.imem <> frontend.io.imem
  io.axi <> axi2to1Arbiter.io.master
}
