package frontend
import chisel3._
import bundle._
import config._
import chisel3.util.Decoupled

class FrontRedirctIO extends MycpuBundle {
  val target = Output(UInt(vaddrWidth.W))
  val flush  = Output(Bool())
  // need addsink to if-stage1 and if-stage2
}

/**
  * out.bits is calculated by alignMask, fromBpu and redirect
  * if redirect.flush = true then out.bits = redirect.target
  * else out.bits = bpu predict target
  * not need Decouple. becasue valid = 1
  * ready not change select logic
  */

// not need valid because always valid
// not need ready becasue input will not change when ready is 0
// but need flash when redirect happen
// or fromBackend connect to stage 1
// and out is a wire signal for bpu
// stage 1 need use reg to save it
// redirect should be merge to one
// bpu modify signal should be to bpu not to preIO
//out:npc
class PreIf extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val redirect  = Flipped(new FrontRedirctIO)
      val fromBpu   = Flipped(new BpuOutIO)
      val alignMask = Input(UInt(fetchNum.W))
    }
    val out = new PreIfOutIO
  })
}
