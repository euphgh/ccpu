package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils._
import chisel3.util.BitPat
import chisel3.util.Cat

class TestBitsUtils extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("WordShift")
  it should "shift word" in {
    test(new Module {
      val io = IO(new Bundle {
        val data      = Input(UInt(4.W))
        val wordIndex = Input(UInt(2.W))
        val out       = Output(UInt(5.W))
      })
      io.out := WordShift(io.data, io.wordIndex, 1)
    }) { c =>
      c.io.data.poke("b0110".U)
      c.io.wordIndex.poke("b01".U)
      c.io.out.expect("b01100".U)
      c.clock.step()

      c.io.data.poke("b0101".U)
      c.io.wordIndex.poke("b10".U)
      c.io.out.expect("b10100".U)
      c.clock.step()
    // c.io.newData.poke("b1101".U)
    // c.io.oldData.poke("b0001".U)
    // c.io.fullMask.poke("b0110".U)
    // c.io.out.expect("b0101".U)
    // c.clock.step()
    }
  }
  behavior.of("MaskData")
  it should "use newdata certain bit to cover old data according to mask" in {
    test(new Module {
      val io = IO(new Bundle {
        val newData  = Input(UInt(4.W))
        val oldData  = Input(UInt(4.W))
        val fullMask = Input(UInt(4.W))
        val out      = Output(UInt(4.W))
      })
      io.out := MaskData(io.oldData, io.newData, io.fullMask)
    }) { c =>
      c.io.newData.poke("b0110".U)
      c.io.oldData.poke("b1011".U)
      c.io.fullMask.poke("b1010".U)
      c.io.out.expect("b0011".U)
      c.clock.step()

      c.io.newData.poke("b1101".U)
      c.io.oldData.poke("b0001".U)
      c.io.fullMask.poke("b0110".U)
      c.io.out.expect("b0101".U)
      c.clock.step()
    }
  }

  behavior.of("MaskExpand")
  it should "expand byte msk to bit msk" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(3.W))
        val out = Output(UInt(24.W))
      })
      io.out := MaskExpand(io.in)
    }) { c =>
      c.io.in.poke("b110".U)
      c.io.out.expect("hffff00".U)
      c.clock.step()
    }
  }

  behavior.of("SignExt")
  it should "signed extend bits to request width if in.length<req" in {
    test(new Module {
      val reqLen = 8
      val io = IO(new Bundle {
        val in  = Input(UInt(5.W))
        val out = Output(UInt(reqLen.W))
      })
      io.out := SignExt(io.in, reqLen)
    }) { c =>
      c.io.in.poke("b10110".U)
      c.io.out.expect("b11110110".U)
      c.clock.step()

      c.io.in.poke("b00101".U)
      c.io.out.expect("b00000101".U)
      c.clock.step()
    }
  }
  it should "hold correspond witdh if in.len>=req.len" in {
    test(new Module {
      val reqLen = 3
      val io = IO(new Bundle {
        val in  = Input(UInt(5.W))
        val out = Output(UInt(reqLen.W))
      })
      io.out := SignExt(io.in, reqLen)
    }) { c =>
      c.io.in.poke("b10110".U)
      c.io.out.expect("b110".U)
      c.clock.step()

      c.io.in.poke("b00101".U)
      c.io.out.expect("b101".U)
      c.clock.step()
    }
  }

  behavior.of("ZeroExt")
  it should "zero extend bits to request width if in.length<req" in {
    test(new Module {
      val reqLen = 8
      val io = IO(new Bundle {
        val in  = Input(UInt(5.W))
        val out = Output(UInt(reqLen.W))
      })
      io.out := ZeroExt(io.in, reqLen)
    }) { c =>
      c.io.in.poke("b10110".U)
      c.io.out.expect("b00010110".U)
      c.clock.step()

      c.io.in.poke("b00101".U)
      c.io.out.expect("b00000101".U)
      c.clock.step()
    }
  }
  it should "hold correspond witdh if in.len>=req.len" in {
    test(new Module {
      val reqLen = 3
      val io = IO(new Bundle {
        val in  = Input(UInt(5.W))
        val out = Output(UInt(reqLen.W))
      })
      io.out := ZeroExt(io.in, reqLen)
    }) { c =>
      c.io.in.poke("b10110".U)
      c.io.out.expect("b110".U)
      c.clock.step()

      c.io.in.poke("b00101".U)
      c.io.out.expect("b101".U)
      c.clock.step()
    }
  }
}
