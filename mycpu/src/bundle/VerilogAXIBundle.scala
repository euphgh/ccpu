package bundle
import chisel3.experimental.dataview._
import chisel3._
import chisel3.util._
import config._

class VerilogAXIBundle extends Bundle {
  val arvalid = Output(Bool())
  val arready = Input(Bool())
  val arid    = Output(UInt(4.W))
  val araddr  = Output(UInt(32.W))
  val arlen   = Output(UInt(4.W))
  val arsize  = Output(UInt(3.W))
  val arburst = Output(BurstType())
  val arlock  = Output(UInt(2.W))
  val arcache = Output(UInt(4.W))
  val arprot  = Output(UInt(3.W))

  val rid    = Input(UInt(4.W))
  val rdata  = Input(UInt(32.W))
  val rresp  = Input(UInt(2.W))
  val rlast  = Input(Bool())
  val rvalid = Input(Bool())
  val rready = Output(Bool())

  val awvalid = Output(Bool())
  val awready = Input(Bool())
  val awid    = Output(UInt(4.W))
  val awaddr  = Output(UInt(32.W))
  val awlen   = Output(UInt(4.W))
  val awsize  = Output(UInt(3.W))
  val awburst = Output(BurstType())
  val awlock  = Output(UInt(2.W))
  val awcache = Output(UInt(4.W))
  val awprot  = Output(UInt(3.W))

  val wid    = Output(UInt(4.W))
  val wdata  = Output(UInt(32.W))
  val wstrb  = Output(UInt(4.W))
  val wlast  = Output(Bool())
  val wvalid = Output(Bool())
  val wready = Input(Bool())

  val bid    = Input(UInt(4.W))
  val bresp  = Input(UInt(2.W))
  val bvalid = Input(Bool())
  val bready = Output(Bool())
}

// We recommend putting DataViews in a companion object of one of the involved types
object AXIBundle {
  implicit val axiView = DataView[VerilogAXIBundle, AxiIO](
    vab => new AxiIO,
    // ar
    _.arvalid -> _.ar.valid,
    _.arready -> _.ar.ready,
    _.arid    -> _.ar.bits.used.id,
    _.araddr  -> _.ar.bits.used.addr,
    _.arlen   -> _.ar.bits.used.len,
    _.arsize  -> _.ar.bits.used.size,
    _.arburst -> _.ar.bits.used.burst,
    _.arlock  -> _.ar.bits.unused.lock,
    _.arcache -> _.ar.bits.unused.cache,
    _.arprot  -> _.ar.bits.unused.prot,
    // r
    _.rid    -> _.r.bits.used.id,
    _.rdata  -> _.r.bits.used.data,
    _.rresp  -> _.r.bits.rresp,
    _.rlast  -> _.r.bits.used.last,
    _.rvalid -> _.r.valid,
    _.rready -> _.r.ready,
    // w
    _.wid    -> _.w.bits.used.id,
    _.wdata  -> _.w.bits.used.data,
    _.wstrb  -> _.w.bits.used.strb,
    _.wlast  -> _.w.bits.used.last,
    _.wvalid -> _.w.valid,
    _.wready -> _.w.ready,
    // b
    _.bid    -> _.b.bits.used.id,
    _.bresp  -> _.b.bits.bresp,
    _.bvalid -> _.b.valid,
    _.bready -> _.b.ready,
    // aw
    _.awvalid -> _.aw.valid,
    _.awready -> _.aw.ready,
    _.awid    -> _.aw.bits.used.id,
    _.awaddr  -> _.aw.bits.used.addr,
    _.awlen   -> _.aw.bits.used.len,
    _.awsize  -> _.aw.bits.used.size,
    _.awburst -> _.aw.bits.used.burst,
    _.awlock  -> _.aw.bits.unused.lock,
    _.awcache -> _.aw.bits.unused.cache,
    _.awprot  -> _.aw.bits.unused.prot
  )
}
