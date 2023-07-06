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
  *      no-branch mispredict-----<if2>
  */
class FrontRedirctIO extends MycpuBundle {
  val target = Output(UInt(vaddrWidth.W))
  val flush  = Output(Bool())
}

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
      val redirect = Flipped(new FrontRedirctIO)
      val fromIf1  = Flipped(new IfStage1ToPreIf)
    }
    val out = new PreIfOutIO
  })
  val if1InPc = io.in.fromIf1.pcVal
  val pc314   = if1InPc(31, 4)
  val alignPC = Mux(if1InPc(4, 2) > 4.U(3.W), Cat(pc314 + 1.U, 0.U(4.W)), Cat(pc314 + 1.U, if1InPc(3, 0)))
  //Cat(io.in.fromIf1.pcVal(31, 4) + 1.U, io.in.fromIf1.pcVal(3, 0)) //Cat(io.in.fromIf1.pcVal(31, 4) + 1.U, 0.U(4.W))
  object PreIfState extends ChiselEnum {
    val normal, keepDest = Value
  }
  import PreIfState._
  val state       = RegInit(normal)
  val brDestSaved = RegInit(0.U(vaddrWidth.W))
  asg(io.out.npc, 0.U(32.W)) //init
  asg(io.out.isDelaySlot, false.B) //init
  switch(state) {
    is(normal) {
      when(io.in.fromIf1.hasBranch && !io.in.fromIf1.dsFetched) {
        state              := Mux(io.in.fromIf1.stage1Rdy, keepDest, normal)
        io.out.isDelaySlot := true.B
        io.out.npc         := alignPC
        brDestSaved        := io.in.fromIf1.predictDst
      }.otherwise {
        io.out.isDelaySlot := false.B
        io.out.npc         := Mux(io.in.fromIf1.hasBranch, io.in.fromIf1.predictDst, alignPC)
      }
    }
    is(keepDest) {
      io.out.isDelaySlot := false.B
      io.out.npc         := brDestSaved
      when(io.in.fromIf1.stage1Rdy) {
        state := normal
      }
    }
  }
  // flush has highest priority
  io.out.flush := io.in.redirect.flush
  when(io.in.redirect.flush) {
    io.out.isDelaySlot := false.B
    io.out.npc         := io.in.redirect.target
    state              := normal
  }
}
