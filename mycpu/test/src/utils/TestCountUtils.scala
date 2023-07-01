package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat

class TestCountUtils extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("CountMask")
  it should "Count 1 and generate UInt with consecutive 1 from bit 0" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := CountMask(io.in)
    }) { c =>
      c.io.in.poke("b000".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b001".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b010".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b011".U)
      c.io.out.expect("b011".U)
      c.clock.step()

      c.io.in.poke("b100".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b101".U)
      c.io.out.expect("b011".U)
      c.clock.step()

      c.io.in.poke("b110".U)
      c.io.out.expect("b011".U)
      c.clock.step()

      c.io.in.poke("b111".U)
      c.io.out.expect("b111".U)
      c.clock.step()
    }
  }
  behavior.of("CountMask.oneHot")
  it should "Save the last 1 of CountMask" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := CountMask.oneHot(io.in)
    }) { c =>
      c.io.in.poke("b000".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b001".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b010".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b011".U)
      c.io.out.expect("b010".U)
      c.clock.step()

      c.io.in.poke("b100".U)
      c.io.out.expect("b001".U)
      c.clock.step()

      c.io.in.poke("b101".U)
      c.io.out.expect("b010".U)
      c.clock.step()

      c.io.in.poke("b110".U)
      c.io.out.expect("b010".U)
      c.clock.step()

      c.io.in.poke("b111".U)
      c.io.out.expect("b100".U)
      c.clock.step()
    }
  }
}
