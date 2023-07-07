package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat
import chisel3.util.Cat
import chisel3.util.Valid
import config._

class TestStoreQ extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("getByteIdx")
  it should "get the valid and newest idx" in {
    test(new Module {
      val io = IO(new Bundle {
        val enqPtr   = Input(UInt(4.W))
        val deqPtr   = Input(UInt(4.W))
        val matchWen = Input(UInt(8.W)) //8项
        val out      = Output(UInt(3.W))
      })
      io.out := StoreQUtils.getByteIndex(io.enqPtr, io.deqPtr, io.matchWen, 8)
    }) { c =>
      c.io.deqPtr.poke("b0001".U) //1
      c.io.enqPtr.poke("b0110".U) //6
      c.io.matchWen.poke("b11111111".U) //1 2 3 4 5
      c.io.out.expect("b101".U)
      c.clock.step()

      c.io.deqPtr.poke("b0101".U) //5
      c.io.enqPtr.poke("b1010".U) //10
      c.io.matchWen.poke("b01010100".U) //5 6 7 1 2
      c.io.out.expect("b110".U)
      c.clock.step()

      c.io.deqPtr.poke("b1010".U) //10->2
      c.io.enqPtr.poke("b1110".U) //14->6
      c.io.matchWen.poke("b00000100".U) //2 3 4 5
      c.io.out.expect("b010".U)
      println(c.io.out.peekInt())
      c.clock.step()

      c.io.deqPtr.poke("b1110".U) //14->6
      c.io.enqPtr.poke("b0100".U) //4
      c.io.matchWen.poke("b11110011".U) // 6 7 0 1 2 3
      c.io.out.expect("b001".U)
      println(c.io.out.peekInt())
      c.clock.step()
    }
  }
}
