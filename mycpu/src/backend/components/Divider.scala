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
}

object Divider {
  def apply(useShift: Boolean = false) = {
    if (useShift) new ShiftDivider
    else new DoubleDivider
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
    val srcs          = io.in.bits.srcs
    val isSign        = io.in.bits.isSign
    val signQ, signR  = RegInit(false.B)
    val cnt           = RegInit(0.U(4.W))
    val leftCnt       = RegInit(16.U(5.W))
    val x, y1, y2, y3 = RegInit(0.U(64.W))
    val xShift        = RegInit(0.U(64.W))
    val quot          = RegInit(0.U(32.W))
    val y1_init       = Cat(0.U(2.W), Mux(srcs(1)(31) && isSign, ~srcs(1) + 1.U, srcs(1)), 0.U(30.W))
    val sub1          = x -& y1
    val sub2          = x -& y2
    val sub3          = x -& y3
    val quotient      = Wire(UWord)
    val reminder      = Wire(UWord)
    // dontTouch(quotient)
    // dontTouch(reminder)

    val idle :: decode :: shift :: work :: finish :: Nil = Enum(5)
    // state
    val state = RegInit(idle)
    assert(~(io.out.valid & state =/= finish))
    io.out.valid := false.B
    switch(state) {
      is(idle) {
        asg(x, Cat(0.U(32.W), Mux(srcs(0)(31) && isSign, ~srcs(0) + 1.U, srcs(0))))
        asg(y1, y1_init)
        asg(y2, (y1_init << 1)(63, 0))
        asg(y3, y1_init + (y1_init << 1)(63, 0))
        asg(signQ, (srcs(0)(31) ^ srcs(1)(31)) && isSign)
        asg(signR, srcs(0)(31) && isSign)
        cnt     := 0.U
        leftCnt := 16.U
        quot    := 0.U
        when(io.in.valid) { state := decode }
      }
      is(decode) {
        val isSub      = x(31, 0) < y1(61, 30)
        val yCountZero = countFirstOne(y1(63, 63 - shiftWidth + 1))
        val cntAdd     = (yCountZero + 1.U) >> 1
        // dontTouch(cntAdd)
        // dontTouch(yCountZero)
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
        // dontTouch(nextLeft)
        // dontTouch(shiftBits)
        // dontTouch(nextCnt)
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
      is(work) {
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
      is(finish) {
        io.out.valid := true.B
        state        := idle
      }
    }
    when(io.flush) {
      state := idle
    }
    asg(quotient, Mux(signQ, ~quot + 1.U, quot))
    val xHigh32 = x(63, 32)
    asg(reminder, Mux(signR, ~xHigh32 + 1.U, xHigh32))
    asg(io.out.bits, Cat(reminder, quotient))
  }

  class DoubleDivider extends Divider {
    val srcs          = io.in.bits.srcs
    val isSign        = io.in.bits.isSign
    val signQ, signR  = RegInit(false.B)
    val cnt           = Counter(16)
    val x, y1, y2, y3 = RegInit(0.U(64.W))
    val quot          = RegInit(0.U(32.W))
    val y1_init       = Cat(0.U(2.W), Mux(srcs(1)(31) && isSign, ~srcs(1) + 1.U, srcs(1)), 0.U(30.W))
    val sub1          = x -& y1
    val sub2          = x -& y2
    val sub3          = x -& y3
    val quotient      = Wire(UWord)
    val reminder      = Wire(UWord)

    val idle :: work :: finish :: Nil = Enum(3)
    // state
    val state = RegInit(idle)
    assert(~(io.out.valid & state =/= finish))
    io.out.valid := false.B
    switch(state) {
      is(idle) {
        asg(x, Cat(0.U(32.W), Mux(srcs(0)(31) && isSign, ~srcs(0) + 1.U, srcs(0))))
        asg(y1, y1_init)
        asg(y2, (y1_init << 1)(63, 0))
        asg(y3, y1_init + (y1_init << 1)(63, 0))
        asg(signQ, (srcs(0)(31) ^ srcs(1)(31)) && isSign)
        asg(signR, srcs(0)(31) && isSign)
        cnt.reset()
        state := Mux(io.flush, idle, Mux(io.in.valid, work, idle))
        when(io.in.valid) {
          // printf(s"req Signal: %b\n", isSign)
          // printf(s"signQ: %b\n", signQ)
          // printf(s"signR: %b\n", signR)
        }
      }
      is(work) {
        asg(
          x,
          MuxCase(
            x,
            Seq(
              !sub3(64) -> sub3(63, 0),
              !sub2(64) -> sub2(63, 0),
              !sub1(64) -> sub1(63, 0)
            )
          )
        )
        y1 := y1 >> 2
        y2 := y2 >> 2
        y3 := y3 >> 2
        asg(
          quot,
          (quot << 2)(31, 0) | Cat(0.U(30.W), Cat(!sub3(64) || !sub2(64), !sub3(64) || (sub2(64) && !sub1(64))))
        )
        val wrap = cnt.inc()
        state := Mux(io.flush, idle, Mux(wrap, finish, work))
        // printf(s"work count :%d\n", cnt.value)
        // printf(s"quotient :%x\n", quot)
        // printf(s"reminder :%x\n", x)
      }
      is(finish) {
        io.out.valid := !io.flush
        state        := idle
        // printf(s"finish count :%d\n", cnt.value)
        // printf(s"quotient :%x\n", quotient)
        // printf(s"reminder :%x\n", reminder)
      }
    }
    asg(quotient, Mux(signQ, ~quot + 1.U, quot))
    val xLow32 = x(31, 0)
    asg(reminder, Mux(signR, ~xLow32 + 1.U, xLow32))
    asg(io.out.bits, Cat(reminder, quotient))
  }

}
