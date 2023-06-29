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

class TLBReadIO extends MycpuBundle {
  val req = Input(TLBIdx)
  val res = Output(new TLBEntry)
}

class TLBWriteIO extends MycpuBundle {
  val req = Valid(new Bundle {
    val idx   = TLBIdx
    val entry = new TLBEntry
  })
}

class TLBProbe extends MycpuBundle {
  val req = Decoupled(UWord) // result is ok in fire cycle
  val res = Output(new Bundle {
    val found = Bool()
    val index = TLBIdx
  })
}
