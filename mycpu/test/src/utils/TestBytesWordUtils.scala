package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat
import chisel3.util.Cat
import BytesWordUtils._

class TestBytesWordUtils extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("word2Bytes")
  it should "change 32-bit word into 4 vec of byte" in {
    test(new Module {
      val io = IO(new Bundle {
        val word = Input(UInt(32.W))
        val out  = Output(Vec(4, UInt(8.W)))
      })
      io.out := word2Bytes(io.word)
    }) { c =>
      c.io.word.poke("h11010100".U)
      c.io.out(0).expect("h00".U)
      c.io.out(1).expect("h01".U)
      c.io.out(2).expect("h01".U)
      c.io.out(3).expect("h11".U)

      c.io.word.poke(511.U(32.W))
      c.io.out(0).expect(255.U(8.W))
      c.io.out(1).expect(1.U(8.W))
      c.io.out(2).expect("h00".U)
      c.io.out(3).expect("h00".U)
    }
  }
  behavior.of("maskWord")
  it should "mask certain Byte of 32-bit Word" in {
    test(new Module {
      val io = IO(new Bundle {
        val word = Input(UInt(32.W))
        val mask = Input(UInt(4.W))
        val out  = Output(Vec(4, UInt(8.W)))
      })
      io.out := maskWord(io.word, io.mask)
    }) { c =>
      c.io.word.poke("h11010100".U)
      c.io.mask.poke("b0101".U)
      c.io.out(0).expect("h00".U)
      c.io.out(1).expect("h00".U)
      c.io.out(2).expect("h01".U)
      c.io.out(3).expect("h00".U)

    }
  }
}
