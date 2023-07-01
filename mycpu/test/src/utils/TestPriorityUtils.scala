package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat

class TestPriorityUtils extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("PriotityMask")
  it should "mask first \"1\" and other bits after it " in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := PriorityMask(io.in)
    }) { c =>
      c.io.in.poke("b000".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b001".U)
      c.io.out.expect("b111".U)
      c.clock.step()

      c.io.in.poke("b011".U)
      c.io.out.expect("b111".U)
      c.clock.step()

      c.io.in.poke("b111".U)
      c.io.out.expect("b111".U)
      c.clock.step()

      c.io.in.poke("b010".U)
      c.io.out.expect("b110".U)
      c.clock.step()

      c.io.in.poke("b110".U)
      c.io.out.expect("b110".U)
      c.clock.step()

      c.io.in.poke("b100".U)
      c.io.out.expect("b100".U)
      c.clock.step()
    }
  }

  behavior.of("PriotityCount")
  it should "count consecutive 1 from bit 0" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := PriorityCount(io.in)
    }) { c =>
      c.io.in.poke("b000".U)
      c.io.out.expect(0.U)
      c.clock.step()

      c.io.in.poke("b001".U)
      c.io.out.expect(1.U)
      c.clock.step()

      c.io.in.poke("b011".U)
      c.io.out.expect(2.U)
      c.clock.step()

      c.io.in.poke("b111".U)
      c.io.out.expect(3.U)
    }
  }
  it should "out any when input not consecutive 1" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := PriorityCount(io.in)
    }) { c =>
      c.io.in.poke("b110".U)
      println("when in is b110, output value :" + c.io.out.peek().litValue)
      c.clock.step()

      c.io.in.poke("b010".U)
      println("when in is b010, output value :" + c.io.out.peek().litValue)
      c.clock.step()

      c.io.in.poke("b101".U)
      println("when in is b101, output value :" + c.io.out.peek().litValue)
      c.clock.step()

    }
  }
  behavior.of("SecondPriEncoder")
  it should "set 1 which is second" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := SecondPriEncoder(io.in)
    }) { c =>
      c.io.in.poke("b011".U)
      c.io.out.expect("b010".U)
      c.clock.step()

      c.io.in.poke("b101".U)
      c.io.out.expect("b100".U)
      c.clock.step()

      c.io.in.poke("b110".U)
      c.io.out.expect("b100".U)
      c.clock.step()

      c.io.in.poke("b111".U)
      c.io.out.expect("b010".U)
      c.clock.step()
    }
  }
  it should "out 0 when no second 1" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(3.W))
      })
      io.out := SecondPriEncoder(io.in)
    }) { c =>
      c.io.in.poke("b000".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b001".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b010".U)
      c.io.out.expect("b000".U)
      c.clock.step()

      c.io.in.poke("b100".U)
      c.io.out.expect("b000".U)
      c.clock.step()
    }
  }

}
