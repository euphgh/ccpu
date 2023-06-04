package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  *  val tagOfMemReqPaddr = UInt(tagWidth.W)
  *
  *  val index            = UInt(cacheIndexWidth.W)
  *  val offset           = UInt(cacheOffsetWidth.W)
  *
  *  val size  = UInt(3.W)
  *  val wWord = UWord)
  *  val wStrb = UInt(4.W)
  *  val memType = MemType()
  */

//TODO:here the entry is type output,change to simple signal is easy
class StoreQueueEntry extends StoreQueueOutIO {
  val valid = Output(Bool())
}

class StoreQueue extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new StoreQueueOutIO))
    val out = Decoupled(new StoreQueueOutIO)
  })
}
