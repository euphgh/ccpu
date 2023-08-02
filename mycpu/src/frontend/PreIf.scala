package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg

/**
  * flush:
  *      exception/eret-----<retire>
  *      mispredict occur-----<exe>
  *      delayslot/target-----<ifStage2>
  */

/**
  * out.bits is calculated by alignMask, fromBpu and redirect
  * if redirect.flush = true then out.bits = redirect.target
  * else out.bits = bpu predict target
  * not need Decouple. becasue valid = 1
  * ready not change select logic
  *
  * not need valid because always valid
  * not need ready becasue input will not change when ready is 0
  * but need flush when redirect happen
  *
  * need a automat in it to save bpu result when only valid
  * branch or jump but not valid delay branch
  * set automat status to use ds pc in next cycle when first branch valid is "10"
  * set automat status to give up ds pc when delaySlotOK is set
  */

class PreIf extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val redirect  = Flipped(new FrontRedirctIO)
      val fromIf1   = Flipped(new IfStage1ToPreIf)
      val isDSredir = Input(Bool())
    }
    val out = new PreIfOutIO
  })
  val if1InPc = io.in.fromIf1.pcVal
  val pc314   = if1InPc(31, 4)
  val alignPC = Mux(if1InPc(4, 2) > 4.U(3.W), Cat(pc314 + 1.U, 0.U(4.W)), Cat(pc314 + 1.U, if1InPc(3, 0)))
  asg(
    io.out.npc,
    MuxCase(
      alignPC,
      Seq(
        io.in.redirect.flush    -> io.in.redirect.target,
        io.in.fromIf1.hasBranch -> io.in.fromIf1.predictDst
      )
    )
  )
  io.out.flush       := io.in.redirect.flush
  io.out.isDelaySlot := io.in.isDSredir
}
