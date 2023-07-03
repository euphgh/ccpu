package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat
import chisel3.util.Cat
import chisel3.util.Valid
import config._

class TestMark extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("mark")
  it should "hold the data until flush" in {
    test(new Module {
      val io = IO(new Bundle {
        val start = Flipped(Valid(UInt(3.W)))
        val end   = Input(Bool())
        val out   = Valid(UInt(3.W))
      })
      val mark = Module(new Mark(UInt(3.W)))
      mark.start <> io.start
      mark.end := io.end
      io.out   := mark.value
    }) { c =>
      //init
      c.io.start.valid.poke(false.B)
      c.clock.step()
      c.io.out.valid.expect(false.B)

      //give valid and bits 1
      c.io.start.valid.poke(true.B)
      c.io.start.bits.poke(1.U(3.W))
      c.clock.step()
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(1.U(3.W))

      //give valid and bits 2,but can't change
      c.io.start.valid.poke(true.B)
      c.io.start.bits.poke(2.U(3.W))
      c.clock.step()
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(1.U(3.W))

      //give flush,unvalid data
      c.io.end.poke(true.B)
      c.clock.step()
      c.io.out.valid.expect(false.B)

      //give valid again,this time the data can be reset
      c.io.end.poke(false.B)
      c.io.start.valid.poke(true.B)
      c.io.start.bits.poke(2.U(3.W))
      c.clock.step()
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(2.U(3.W))
    }
  }
}
