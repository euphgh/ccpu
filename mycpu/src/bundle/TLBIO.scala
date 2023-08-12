package bundle

import chisel3.util._
import chisel3._
import config._

class TLBEntry extends MycpuBundle {
  val g    = Bool()
  val v0   = Bool()
  val v1   = Bool()
  val d0   = Bool()
  val d1   = Bool()
  val c0   = UInt(3.W)
  val c1   = UInt(3.W)
  val pfn0 = UInt(20.W)
  val pfn1 = UInt(20.W)
  val vpn2 = UInt(19.W)
  val asid = UInt(8.W)
}

object TLBSearchRes {
  def dir(cattr: CCAttr.Type, ptag: UInt) = {
    val dirRes = Wire(new TLBSearchRes)
    dirRes.refill := false.B
    dirRes.hit    := true.B
    dirRes.dirty  := true.B
    dirRes.ccAttr := cattr
    dirRes.pTag   := ptag
    require(ptag.getWidth == 20)
    dirRes
  }
}

class TLBSearchRes extends MycpuBundle {
  val pTag   = UInt(tagWidth.W)
  val hit    = Bool()
  val dirty  = Bool() // dirty bits
  val refill = Bool() // no match by addr and asid
  val ccAttr = CCAttr()
}

class TLBSearchIO extends MycpuBundle {
  val req = Valid(UWord)
  val res = Input(new TLBSearchRes)
}
