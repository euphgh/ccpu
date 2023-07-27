import chisel3._
import chisel3.util._
import config._
import frontend._
import backend._
import utils._
import bundle._
import bundle.AXIBundle._
import chisel3.experimental.dataview._
import chisel3.experimental._
import chisel3.util._

class CCPU extends MycpuModule {
  val extInt            = IO(Input(UInt(6.W)))
  val bus               = IO(new VerilogAXIBundle)
  val axi               = bus.viewAs[AxiIO]
  val debug_wb_pc       = IO(Output(UWord))
  val debug_wb_rf_wen   = IO(Output(UInt(4.W)))
  val debug_wb_rf_wnum  = IO(Output(UInt(5.W)))
  val debug_wb_rf_wdata = IO(Output(UWord))
  extInt.suggestName("ext_int")
  debug_wb_pc       := 0.U
  debug_wb_rf_wdata := 0.U
  debug_wb_rf_wen   := 0.U
  debug_wb_rf_wnum  := 0.U

  val frontend = Module(new Frontend)
  val backend  = Module(new Backend)
  val fIO      = frontend.io
  val bIO      = backend.io
  val flush    = bIO.redirectFront.flush

  asg(fIO.bpuUpdateIn, bIO.bpuUpdate)
  asg(fIO.redirect, bIO.redirectFront)
  asg(bIO.extInt, extInt)
  bIO.fronTlbSearch <> fIO.tlbSearch
  IbfConnectDper(new InstBufferOutIO, fIO.out, bIO.in, bIO.dperOutFireNum, flush)

  val axi2to1Arbiter = Module(new Axi2to1Arbiter)
  axi2to1Arbiter.io.dmem <> backend.io.dram
  axi2to1Arbiter.io.imem <> frontend.io.imem
  axi <> axi2to1Arbiter.io.master
}
