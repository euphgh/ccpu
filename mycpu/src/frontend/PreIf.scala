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
      val fromBpu   = Input(Vec(fetchNum, new PredictResultBundle))
      val alignMask = Input(UInt(fetchNum.W))
      val fire      = Input(Bool())
      val pcVal     = Input(UWord)
    }
    val out = new PreIfOutIO
  })
  val validBranch = Wire(UInt(fetchNum.W))
  val bpuInfo     = Vec(fetchNum, new PredictResultBundle)
  val takeMask    = Wire(UInt(fetchNum.W))
  (0 to 3).foreach(i => {
    bpuInfo(i)  := io.in.fromBpu(i)
    takeMask(i) := bpuInfo(i).counter > 1.U
    val isTakeBr = takeMask(i) && bpuInfo(i).brType === BranchType.b
    validBranch(i) := (isTakeBr || BranchType.isJump(bpuInfo(i).brType)) && io.in.alignMask(i)
  })
  val preDest = Wire(UWord)
  val takeDs  = Wire(Bool())
  (preDest, takeDs) := PriorityMux(
    Seq(
      validBranch(0) -> (bpuInfo(0).target, io.in.alignMask(1)),
      validBranch(1) -> (bpuInfo(1).target, io.in.alignMask(2)),
      validBranch(2) -> (bpuInfo(2).target, io.in.alignMask(3)),
      validBranch(3) -> (bpuInfo(3).target, false.B)
    )
  )
  val hasValidBranch = validBranch.orR
  val alignPC        = Cat(io.in.pcVal(31, 4) + 1.U, "b0000".U)
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
