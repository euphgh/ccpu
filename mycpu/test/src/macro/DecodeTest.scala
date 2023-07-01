import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import utils.PatRand
import chisel3.util._
import chisel3._
import decodemacro._
import frontend.AllInsts
import chisel3.util.experimental.decode._
import config._

@MacroDecode
class TestBundle extends Bundle {
  val srcType = SRCType()
  val brType  = BranchType.NON
  val dstType = DSTType()
  val fuType  = ChiselFuType()
}

class DecodeTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("DecodeMacro")
  it should "same with specified result" in {
    test(new Module {
      val io = IO(new Bundle {
        val in  = Input(UInt(32.W))
        val out = Output(new TestBundle)
      })
      val logic = Wire(new TestBundle)
      logic.decode(io.in, AllInsts(), AllInsts.default(), QMCMinimizer)
      io.out := logic
    }) { c =>
      c.io.in.poke(PatRand(AllInsts.ADD))
      c.io.out.srcType.expect(SRCType.RSRT)
      c.io.out.brType.expect(BranchType.NON)
      c.io.out.dstType.expect(DSTType.toRD)
      c.io.out.fuType.expect(ChiselFuType.SubALU)
      c.clock.step()

      c.io.in.poke(PatRand(AllInsts.JAL))
      c.io.out.srcType.expect(SRCType.noSRC)
      c.io.out.brType.expect(BranchType.JAL)
      c.io.out.dstType.expect(DSTType.to31)
      c.io.out.fuType.expect(ChiselFuType.MainALU)
    }
  }
}
