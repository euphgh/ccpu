package bundle
import config._
import chisel3._
import chisel3.util.Decoupled

object BurstType extends ChiselEnum {
  val FIXED    = Value("b00".U)
  val INCR     = Value("b01".U)
  val WRAP     = Value("b10".U)
  val RESERVED = Value("b11".U)
}
object SizeType extends ChiselEnum {
  val Byte  = Value("b000".U)
  val Half  = Value("b001".U)
  val Word  = Value("b010".U)
  val other = Value("b100".U) //填宽度用
}

class ARChannel(val idLen: Int = 4) extends MycpuBundle {
  val id    = UInt(idLen.W)
  val addr  = UWord
  val len   = UInt(4.W) // burst number
  val size  = UInt(3.W) // bytes number - 1
  val burst = BurstType()
}

class RChannel(val idLen: Int = 4) extends MycpuBundle {
  val id   = UInt(idLen.W)
  val data = UWord
  val last = Bool()
}

class AWChannel(val idLen: Int = 4) extends MycpuBundle {
  val id    = UInt(idLen.W)
  val addr  = UWord
  val len   = UInt(4.W) // burst number
  val size  = UInt(3.W) // bytes number - 1
  val burst = BurstType()

}

class WChannel(val idLen: Int = 4) extends MycpuBundle {
  val id   = UInt(idLen.W)
  val data = UWord
  val strb = UInt((vaddrWidth / 8).W)
  val last = Bool()
}

class BChannel(val idLen: Int = 4) extends MycpuBundle {
  val id = UInt(idLen.W)
}

class DramReadIO extends MycpuBundle {
  val ar = Decoupled(new ARChannel)
  val r  = Flipped(Decoupled(new RChannel))
}
class DramWriteIO extends MycpuBundle {
  val aw = Decoupled(new AWChannel)
  val w  = Decoupled(new WChannel)
  val b  = Flipped(Decoupled(new BChannel))
}

class DramIO extends MycpuBundle {
  val ar = Decoupled(new ARChannel)
  val r  = Flipped(Decoupled(new RChannel))
  val aw = Decoupled(new AWChannel)
  val w  = Decoupled(new WChannel)
  val b  = Flipped(Decoupled(new BChannel))
  def whenARfire(whenFire: => Any) = {
    // axi ar req valid
    ar.valid := true.B
    // only when next cycle can set r.ready
    r.ready := false.B
    // when axi allow ar
    when(ar.fire) {
      // should change state in whenFire
      // next state should set ar.valid = 0
      whenFire
    }
  }
  def whenRfire(whenFire: => Any) = {
    // unvalid port
    ar.valid := false.B
    // axi rready set
    r.ready := true.B
    // should watch last in whenFire
    when(r.fire) {
      whenFire
    }
  }
  def whenAWfire(whenFire: => Any) = {
    // axi ar req valid
    aw.valid := true.B
    // for simple, not set w.valid before aw.fire
    w.valid := false.B
    // should change state in whenFire
    // next state should set aw.valid = 0
    when(aw.fire) {
      whenFire
    }
  }
  def whenWfire(whenFire: => Any) = {
    // unset aw.valid
    aw.valid := false.B
    // set aw.valid
    w.valid := true.B
    // for simple, not set b.ready before w.fire
    b.ready := false.B
    // should watch last
    when(w.fire) {
      whenFire
    }
  }
  def whenBfire(whenFire: => Any) = {
    w.valid := false.B
    b.ready := true.B
    when(b.fire) {
      whenFire
    }
  }
}
