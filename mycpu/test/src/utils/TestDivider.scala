package utils
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils.PatRand
import chisel3.util.BitPat
import backend.components.Divider
import scala.util.Random
import UnsignUtils._
import backend.components._

class TestDivider extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Divider")

  val annos = Seq(VerilatorBackendAnnotation)
  it should "calculate in 34" in {
    val rand = new Random(0)
    def refDivide(src0: BigInt, src1: BigInt, isSign: Boolean): (BigInt, BigInt) = {
      if (isSign) (src0 / src1, src0 % src1)
      else src0 /% src1
    }
    test(new DivierTest16).withAnnotations(annos) { dut =>
      dut.reset.poke(true.B)
      dut.clock.step()
      dut.reset.poke(false.B)
      (0 to 10).foreach(i => {
        dut.io.in.valid.poke(true.B)

        val isSign = rand.nextInt(2) == 0
        dut.io.in.bits.isSign.poke(isSign.B)

        val bsrcs = (0 until 2).map(i => BigInt(32, rand))
        val usrcs = bsrcs.map(_.U)
        dut.io.in.bits.srcs(0).poke(usrcs(0))
        dut.io.in.bits.srcs(1).poke(usrcs(1))
        var cnt = 0
        do {
          dut.clock.step()
          cnt = cnt + 1
        } while (dut.io.out.valid.peekBoolean() == false && cnt <= 34)
        val dutQ = dut.io.out.bits.quotient.peek().litValue
        val dutR = dut.io.out.bits.reminder.peek().litValue
        dut.clock.step()
        import java.math.BigInteger

        if (cnt > 34) assert(false, "cycle too long")
        val (refQ, refR) = refDivide(bsrcs(0), bsrcs(1), isSign)

        println(s"HEX: ${toUsHex(bsrcs(0))} % ${toUsHex(bsrcs(1))} = ${toUsHex(refQ)} ... ${toUsHex(refR)}")
        if (isSign) {
          println(s"  S: ${bsrcs(0)} % ${bsrcs(1)} = $refQ ... $refR")
        } else {
          println(s"  U: ${toUsDec(bsrcs(0))} % ${toUsDec(bsrcs(1))} = ${toUsDec(refQ)} ... ${toUsDec(refR)}")
        }

        assert(dutQ == refQ, s"dut is $dutQ")
        assert(dutR == refR, s"dut is $dutR")
        println(s" (use $cnt cycle)")
      })
    }
  }
}
