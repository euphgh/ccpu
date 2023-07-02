package bundle
import config._
import chisel3._
import chisel3.util.Decoupled

class ArwUnused extends MycpuBundle {
  val lock  = UInt(2.W)
  val cache = UInt(4.W)
  val prot  = UInt(3.W)
}

class AxiArChannel(var idLen: Int = 4) extends MycpuBundle {
  val used   = new ARChannel(idLen)
  val unused = new ArwUnused
}

class AxiRChannel(var idLen: Int = 4) extends MycpuBundle {
  val used  = new RChannel(idLen)
  val rresp = UInt(2.W)
}

class AxiAWChannel(var idLen: Int = 4) extends MycpuBundle {
  val used   = new AWChannel(idLen)
  val unused = new ArwUnused
}

class AxiWChannel(var idLen: Int = 4) extends MycpuBundle {
  val used = new WChannel(idLen)
}

class AxiBChannel(var idLen: Int = 4) extends MycpuBundle {
  val used  = new BChannel(idLen)
  val bresp = UInt(2.W)
}

class AxiIO extends MycpuBundle {
  val ar = Decoupled(new AxiArChannel)
  val r  = Flipped(Decoupled(new AxiRChannel))
  val aw = Decoupled(new AxiAWChannel)
  val w  = Decoupled(new AxiWChannel)
  val b  = Flipped(Decoupled(new AxiBChannel))
}
