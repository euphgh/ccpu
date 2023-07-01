import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils.PatRand
import chisel3.util.BitPat

class TestPatRandom extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("BitPatRandom")
  it should "give same with 0 or 1 and random in char '?'" in {
    test(new Module {
      val io   = IO(new Bundle {})
      val out0 = PatRand(BitPat("b?11"))
      assert(out0.litValue == 3 || out0.litValue == 7)
      val out1 = PatRand(BitPat("b10?"))
      assert(out1.litValue == 4 || out1.litValue == 5)
    }) { c => }
  }
}
