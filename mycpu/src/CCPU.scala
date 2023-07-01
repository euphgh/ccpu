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
    val dram   = new DramIO
  })
  val frontend = new Frontend
  val backend  = new Backend
  val fIO      = frontend.io
  val bIO      = backend.io
  val flush    = bIO.redirectFront.flush

  asg(fIO.bpuUpdateIn, bIO.bpuUpdate)
  asg(fIO.redirect, bIO.redirectFront)
  asg(bIO.extInt, io.extInt)
  bIO.fronTlbSearch <> fIO.tlbSearch
  IbfConnectDper(new InstBufferOutIO, fIO.out, bIO.in, bIO.dperOutFireNum, flush)

  //TODO:访存和ICACHE

}
