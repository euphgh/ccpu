package backend.components

import config._
import backend._
import chisel3._
import chisel3.util._
import utils._
import chisel3.experimental.ExtModule
import chisel3.util.Reverse._

abstract class Divider extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
    }))
    val out   = Valid(UInt(64.W))
    val flush = Input(Bool())
  })
  val srcs          = io.in.bits.srcs
  val isSign        = io.in.bits.isSign
  val signQ, signR  = RegInit(false.B)
  val x, y1, y2, y3 = RegInit(0.U(64.W))
  val cnt           = RegInit(0.U(4.W))
  val quot          = RegInit(0.U(32.W))
  val y1_init       = Cat(0.U(2.W), Mux(srcs(1)(31) && isSign, ~srcs(1) + 1.U, srcs(1)), 0.U(30.W))
  val sub1          = x -& y1
  val sub2          = x -& y2
  val sub3          = x -& y3
  val quotient      = Wire(UWord)
  val reminder      = Wire(UWord)
  // state
  val idle :: decode :: shift :: work :: finish :: Nil = Enum(5)
  val state                                            = RegInit(idle)

  assert(~(io.out.valid & state =/= finish))
  asg(quotient, Mux(signQ, ~quot + 1.U, quot))
  val xHigh32 = x(63, 32)
  asg(reminder, Mux(signR, ~xHigh32 + 1.U, xHigh32))
  asg(io.out.valid, false.B)
  asg(io.out.bits, Cat(reminder, quotient))
  def acceptReq(nextState: UInt) = {
    asg(x, Cat(0.U(32.W), Mux(srcs(0)(31) && isSign, ~srcs(0) + 1.U, srcs(0))))
    asg(y1, y1_init)
    asg(y2, (y1_init << 1)(63, 0))
    asg(y3, y1_init + (y1_init << 1)(63, 0))
    asg(signQ, (srcs(0)(31) ^ srcs(1)(31)) && isSign)
    asg(signR, srcs(0)(31) && isSign)
    asg(quot, 0.U(32.W))
    cnt := 0.U
    when(io.in.valid) { state := nextState }
  }
  def working = {
    asg(
      x,
      (MuxCase(
        x,
        Seq(
          !sub3(64) -> sub3(63, 0),
          !sub2(64) -> sub2(63, 0),
          !sub1(64) -> sub1(63, 0)
        )
      ) << 2)(63, 0)
    )
    asg(
      quot,
      (quot << 2)(31, 0) | Cat(0.U(30.W), Cat(!sub3(64) || !sub2(64), !sub3(64) || (sub2(64) && !sub1(64))))
    )
    val wrap = cnt.andR
    cnt   := cnt + 1.U
    state := Mux(wrap, finish, work)
  }
  def finishing = {
    io.out.valid := true.B
    state        := idle
  }
  def flushReset = {
    when(io.flush) {
      state := idle
    }
  }
}

object Divider {
  def apply(useShift: Boolean = false) = {
    if (useShift) new ShiftDivider
    else new DoubleDivider
  }

  class DoubleDivider extends Divider {
    switch(state) {
      is(idle) { acceptReq(work) }
      is(work) { working }
      is(finish) { finishing }
    }
    // high priority, should place last
    flushReset
  }

  class ShiftDivider extends Divider {
    val shiftWidth = 8
    def countFirstOne(data: UInt) = {
      val res = UInt((log2Ceil(shiftWidth) + 1).W)
      require(data.getWidth == shiftWidth)
      Mux(data.orR, PriorityEncoder(Reverse(data)), shiftWidth.U)
    }
    def shiftByBits(data: UInt, bits: UInt) = {
      Mux1H((1 to (shiftWidth / 2)).map(i => { (bits === i.U) -> (data << (i * 2)) }))
    }
    val leftCnt = RegInit(16.U(5.W))
    val xShift  = RegInit(0.U(64.W))

    switch(state) {
      is(idle) {
        acceptReq(decode)
        leftCnt := 16.U
      }
      is(decode) {
        val isSub      = x(31, 0) < y1(61, 30)
        val yCountZero = countFirstOne(y1(63, 63 - shiftWidth + 1))
        val cntAdd     = (yCountZero + 1.U) >> 1
        when(isSub) {
          state := finish
          x     := x << 32
        }.elsewhen(shiftWidth.U > yCountZero + 1.U) {
          state  := shift
          xShift := x << (cntAdd << 1)
        }.otherwise {
          state := work
        }
      }
      is(shift) {
        val xFirstOne = countFirstOne(xShift(63, 63 - shiftWidth + 1))
        val subBits   = ((xFirstOne & ~1.U((log2Ceil(shiftWidth) + 1).W))) >> 1
        val shiftBits = Mux(subBits < leftCnt, subBits, leftCnt)
        val nextCnt   = cnt + shiftBits
        val nextLeft  = leftCnt - shiftBits
        cnt     := nextCnt
        leftCnt := nextLeft
        x       := shiftByBits(x, shiftBits)
        xShift  := shiftByBits(xShift, shiftBits)
        when(nextLeft === 0.U) {
          state := finish
        }.elsewhen(shiftBits < (shiftWidth >> 1).U) {
          state := work
        }
      }
      is(work) { working }
      is(finish) { finishing }
    }
    // high priority, should place last
    flushReset
  }
}
