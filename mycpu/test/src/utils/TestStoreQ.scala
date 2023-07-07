package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat
import chisel3.util.Cat
import chisel3.util.Valid
import config._
import chisel3.util.UIntToOH

class TestStoreQ extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("getByteIdx")
  it should "get the valid and newest idx" in {
    test(new Module {
      val io = IO(new Bundle {
        val enqPtr   = Input(UInt(4.W))
        val deqPtr   = Input(UInt(4.W))
        val matchWen = Input(UInt(8.W)) //8项
        val out      = UInt(3.W)
        val valid    = Bool()
      })
      io.out   := StoreQUtils.getByteIndex(io.enqPtr, io.deqPtr, io.matchWen, 8)._1
      io.valid := StoreQUtils.getByteIndex(io.enqPtr, io.deqPtr, io.matchWen, 8)._2

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
      c.io.matchWen.poke("b00001001".U) //2 3 4 5
      println(c.io.out.peekInt())
      c.io.out.expect("b011".U)
      c.clock.step()

      c.io.deqPtr.poke("b1110".U) //14->6
      c.io.enqPtr.poke("b0100".U) //4
      c.io.matchWen.poke("b11110011".U) // 6 7 0 1 2 3
      c.io.out.expect("b001".U)
      println(c.io.out.peekInt())
      c.clock.step()

      c.io.deqPtr.poke("b1".U) //14->6
      c.io.enqPtr.poke("b1".U) //4
      c.io.matchWen.poke("b01011111".U) // 6 7 0 1 2 3
      c.io.out.expect("b00000000".U)
      c.io.valid.expect(false.B)
      println(c.io.out.peekInt())
      c.clock.step()
    }
  }
  behavior.of("decode version")
  it should "same with select version" in {
    test(new Module {
      val io = IO(new Bundle {
        val enqPtr     = Input(UInt(4.W))
        val deqPtr     = Input(UInt(4.W))
        val matchWen   = Input(UInt(8.W)) //8项
        val resAssert  = Output(Bool())
        val selVersion = Output(UInt(8.W))
        val decVersion = Output(UInt(8.W))
      })
      val selRes     = StoreQUtils.getByteIndex(io.enqPtr, io.deqPtr, io.matchWen, 8)
      val selValid   = selRes._2
      val selVersion = Mux(selValid, UIntToOH(selRes._1), 0.U(8.W))
      val foo        = new PtrPriority(8)
      val decVersion = foo.codeOH(io.deqPtr, io.enqPtr, io.matchWen)
      io.selVersion := selVersion
      io.decVersion := decVersion
      io.resAssert  := selVersion === decVersion
    }) { c =>
      {
        (0 until 16).foreach(deq => {
          (0 to 8).foreach(gap => {
            c.io.deqPtr.poke(deq.U)
            val enq = (deq + gap) % 16
            c.io.enqPtr.poke(enq.U)
            (0 until 16).foreach(k => {
              val matchWen   = PatRand(BitPat("b????????"))
              val selVersion = c.io.selVersion.peekInt()
              val decVersion = c.io.decVersion.peekInt()
              c.io.matchWen.poke(matchWen)
              if (c.io.resAssert.peekBoolean() == false) {
                println(s"deq: $deq")
                println(s"enq: $enq")
                println(s"matchWen: ${matchWen.litValue.toString(2)}")
                println(s"selVersion: ${selVersion}")
                println(s"decVersion: ${decVersion}")
                if (selVersion == decVersion) println("Error Assert")
                else c.io.resAssert.expect(true.B)
              }
              c.clock.step()
            })
          })
        })
      }
    }
  }
}
