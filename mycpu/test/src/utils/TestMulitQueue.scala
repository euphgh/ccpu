package utils
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils.PatRand
import chisel3.util.BitPat
import os.truncate

class TestMulitQueue extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Normal MulitQueue")
  it should "in multi and out multi" in {
    test(new MultiQueue(2, 3, UInt(3.W), 8, false)) { dut =>
      // ========================= clock 0 ======================
      // push 0 and 1
      dut.io.push(0).ready.expect(true.B)
      dut.io.push(0).valid.poke(true.B)
      dut.io.push(0).bits.poke(0.U)

      dut.io.push(0).ready.expect(true.B)
      dut.io.push(1).valid.poke(true.B)
      dut.io.push(1).bits.poke(1.U)
      // pop not ready
      dut.io.pop(0).ready.poke(false.B)
      dut.io.pop(1).ready.poke(false.B)
      dut.io.pop(2).ready.poke(false.B)
      dut.clock.step()

      // ========================= clock 1 ======================
      // test pop out, but not pop any
      dut.io.pop(0).valid.expect(true.B)
      dut.io.pop(0).bits.expect(0.U)
      dut.io.pop(1).valid.expect(true.B)
      dut.io.pop(1).bits.expect(1.U)
      dut.io.pop(2).valid.expect(false.B)
      // push 2 and 3
      dut.io.push(0).ready.expect(true.B)
      dut.io.push(0).valid.poke(true.B)
      dut.io.push(0).bits.poke(2.U)
      dut.io.push(1).ready.expect(true.B)
      dut.io.push(1).valid.poke(true.B)
      dut.io.push(1).bits.poke(3.U)
      dut.clock.step()

      // ========================= clock 2 ======================
      // test pop out
      dut.io.pop(0).valid.expect(true.B)
      dut.io.pop(0).bits.expect(0.U)
      dut.io.pop(1).valid.expect(true.B)
      dut.io.pop(1).bits.expect(1.U)
      dut.io.pop(2).valid.expect(true.B)
      dut.io.pop(2).bits.expect(2.U)
      // pop all ready
      dut.io.pop(0).ready.poke(true.B)
      dut.io.pop(1).ready.poke(true.B)
      dut.io.pop(2).ready.poke(true.B)
      // push 4
      dut.io.push(0).ready.expect(true.B)
      dut.io.push(0).valid.poke(true.B)
      dut.io.push(0).bits.poke(4.U)
      dut.io.push(1).ready.expect(true.B)
      dut.io.push(1).valid.poke(false.B)
      dut.clock.step()

      // ========================== clock 3 ======================
      // test pop out
      dut.io.pop(0).valid.expect(true.B)
      dut.io.pop(0).bits.expect(3.U)
      dut.io.pop(1).valid.expect(true.B)
      dut.io.pop(1).bits.expect(4.U)
      dut.io.pop(2).valid.expect(false.B)
      // pop not ready
      dut.io.pop(0).ready.poke(false.B)
      dut.io.pop(1).ready.poke(false.B)
      dut.io.pop(2).ready.poke(false.B)
    }
  }
  it should "not push ready when full" in {
    test(new MultiQueue(4, 2, UInt(4.W), 8, false)) { dut =>
      assert(dut.ptrWidth == 4)
      // ========================= clock 0 ======================
      // push 0, 1, 2, 3
      (0 until 4).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke(i.U)
        dut.io.push(i).ready.expect(true.B)
      })
      // do not pop
      (0 until 2).foreach(i => {
        dut.io.pop(i).ready.poke(false.B)
        dut.io.pop(i).valid.expect(false.B)
      })
      dut.clock.step()

      // ========================= clock 1 ======================
      // push 4, 5, 6
      (0 until 3).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke((4 + i).U)
        dut.io.push(i).ready.expect(true.B)
      })
      dut.io.push(3).valid.poke(false.B)
      dut.io.push(3).ready.expect(true.B)
      dut.clock.step()

      // ========================= clock 2 ======================
      // push 7, 8, 9 ,10
      // only 7 is ready
      dut.io.push(0).valid.poke(true.B)
      dut.io.push(0).bits.poke(7.U)
      dut.io.push(0).ready.expect(true.B)
      (1 until 4).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke((8 + i).U)
        dut.io.push(i).ready.expect(false.B)
      })
      dut.clock.step()
      // ========================= clock 3 ========================
      // can not push for full
      (0 until 4).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke(i.U)
        dut.io.push(i).ready.expect(false.B)
      })
      (0 until 2).foreach(i => {
        dut.io.pop(i).valid.expect(true.B)
        dut.io.pop(i).bits.expect(i.U)
      })
    }
  }
  it should "not pop valid when empty" in {
    test(new MultiQueue(4, 2, UInt(4.W), 8, false)) { dut =>
      assert(dut.ptrWidth == 4)
      // ========================= clock 0 ======================
      // not push
      (0 until 4).foreach(i => {
        dut.io.push(i).valid.poke(false.B)
      })
      // noy pop
      (0 until 2).foreach(i => {
        dut.io.pop(i).ready.poke(false.B)
        dut.io.pop(i).valid.expect(false.B)
      })
      dut.clock.step()
    }
  }
  behavior.of("Normal MulitQueue")
  it should "not ready push when space not enough" in {
    test(new MultiQueue(4, 2, UInt(4.W), 8, true)) { dut =>
      assert(dut.ptrWidth == 4)
      // ========================= clock 0 ======================
      // push 0, 1, 2, 3
      (0 until 4).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke(i.U)
        dut.io.push(i).ready.expect(true.B)
      })
      // do not pop
      (0 until 2).foreach(i => {
        dut.io.pop(i).ready.poke(false.B)
        dut.io.pop(i).valid.expect(false.B)
      })
      dut.clock.step()

      // ========================= clock 1 ======================
      // push 4, 5, 6
      (0 until 3).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke((4 + i).U)
        dut.io.push(i).ready.expect(true.B)
      })
      dut.io.push(3).valid.poke(false.B)
      dut.io.push(3).ready.expect(true.B)
      dut.clock.step()

      // ========================= clock 2 ======================
      // push 7, 8, 9 ,10
      // all can not ready
      (0 until 4).foreach(i => {
        dut.io.push(i).valid.poke(true.B)
        dut.io.push(i).bits.poke((8 + i).U)
        dut.io.push(i).ready.expect(false.B)
      })
      dut.clock.step()
    }
  }
}
