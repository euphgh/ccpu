package bundle
import bundle._
import config._
import chisel3._
import chisel3.util.Decoupled

object BurstType extends ChiselEnum {
  val FIXED    = Value("b00".U)
  val INCR     = Value("b01".U)
  val WRAP     = Value("b10".U)
  val RESERVED = Value("b11".U)
}

class ARChannel(var idLen: Int = 4) extends MycpuBundle {
  val id    = UInt(idLen.W)
  val addr  = UWord
  val len   = UInt(8.W) // burst number
  val size  = UInt(3.W) // bytes number - 1
  val burst = BurstType()
}

class RChannel(var idLen: Int = 4) extends MycpuBundle {
  val id   = UInt(idLen.W)
  val data = UWord
  val last = Bool()
}

class AWChannel(var idLen: Int = 4) extends MycpuBundle {
  val id    = UInt(idLen.W)
  val addr  = UWord
  val len   = UInt(8.W) // burst number
  val size  = UInt(3.W) // bytes number - 1
  val burst = BurstType()

}

class WChannel(var idLen: Int = 4) extends MycpuBundle {
  val id   = UInt(idLen.W)
  val data = UWord
  val strb = UInt((vaddrWidth / 8).W)
  val last = Bool()
}

class BChannel(var idLen: Int = 4) extends MycpuBundle {
  val id = UInt(idLen.W)
}

class DramReadIO {
  val ar = Decoupled(new ARChannel)
  val r  = Flipped(Decoupled(new ARChannel))
}

class DramIO {
  val ar = Decoupled(new ARChannel)
  val r  = Flipped(Decoupled(new ARChannel))
  val aw = Decoupled(new AWChannel)
  val w  = Decoupled(new WChannel)
  val b  = Flipped(Decoupled(new BChannel))
}
