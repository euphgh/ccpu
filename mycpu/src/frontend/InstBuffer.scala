package frontend
import chisel3._
import bundle._
import config._
import chisel3.util.Decoupled

/**
  * implement instbuffer in this stage, can not use queue api
  * because api only support single in and single out
  *
  * move the tail according to the "validNum"
  * write fetchNum insts in instBuffer at one cycle
  * decode the dequeue insts(combination logic) at current cycle
  * but decode logic can not too long because deq is a 16to1mux
  * only decode opcode, src1, src2, dest
  * opcode is used to dispatch select, src and dest are used to rename
  * more complex decode is placed in next cycle
  *
  * To simplify rename and dispatch, InstBuffer only issue 3 or 2 inst,
  * can not chage issue number by decode result
  * in rename stage, if 3 insts can not dispatch or rename for resouce conflict
  * InstBuffer will stop issue until 3 inst has been dispatched
  * it can simplify InstBuffer issue logic from 48to1mux to 16to1 mux
  * the logic of reg should be write in this module
  *
  * out[x].ready are come from one singal, if ready, will issue all valid inst
  */
class InstBuffer extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new IfStage2OutIO))
    val out = Vec(decodeNum, Decoupled(new InstBufferOutIO))
  })
}
