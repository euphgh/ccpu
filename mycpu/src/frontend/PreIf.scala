package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import chisel3.experimental.conversions._

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
      val fromBpu   = Flipped(new BpuOutIO)
      val alignMask = Input(UInt(fetchNum.W))
      val fire      = Input(Bool())
      val pcVal     = Input(UWord)
    }
    val out = new PreIfOutIO
  })
  val validBranch = UInt(4.W)
  (0 to 3).map(i => {
    // is branch inst and pht predict take
    val isTakeBr = io.in.fromBpu.takenMask(i) && io.in.fromBpu.brType(i) === BranchType.b
    // predict take branch or jump is valid branch
    validBranch(i) := (isTakeBr || BranchType.isJump(io.in.fromBpu.brType(i))) && io.in.alignMask(i)
  })
  val preDest = Wire(UWord)
  val takeDs  = Wire(Bool())
  (preDest, takeDs) := PriorityMux(
    Seq(
      validBranch(0) -> (io.in.fromBpu.predictTarget(0), io.in.fromBpu.takenMask(1)),
      validBranch(1) -> (io.in.fromBpu.predictTarget(1), io.in.fromBpu.takenMask(2)),
      validBranch(2) -> (io.in.fromBpu.predictTarget(2), io.in.fromBpu.takenMask(3)),
      validBranch(3) -> (io.in.fromBpu.predictTarget(3), false.B)
    )
  )
  val hasValidBranch = validBranch.orR
  val alignPC        = Cat(io.in.pcVal(31, 2) + 1.U, "b00".U)
  object PreIfState extends ChiselEnum {
    val normal, keepDest = Value
  }
  import PreIfState._
  val state       = RegInit(normal)
  val brDestSaved = RegInit(0.U, UWord)
  switch(state) {
    is(normal) {
      when(hasValidBranch && !takeDs) {
        state              := Mux(io.in.fire, keepDest, normal)
        io.out.isDelaySlot := true.B
        io.out.npc         := alignPC
        brDestSaved        := preDest
      }.otherwise {
        io.out.isDelaySlot := false.B
        io.out.npc         := Mux(hasValidBranch, preDest, alignPC)
      }
    }
    is(keepDest) {
      io.out.isDelaySlot := false.B
      io.out.npc         := brDestSaved
      when(io.in.fire) {
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
