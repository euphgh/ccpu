import circt.stage._
import backend._
import config.FuType
import cache._
import Chisel.UInt
import frontend.Dispatcher
import frontend._
import backend.components.Multiplier
import config.MycpuModule
import chisel3._
import bundle.TLBSearchIO
object Elaborate extends App {
  def top       = new CCPU
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}
class WrapIfStage1 extends MycpuModule {
  val update = IO(Flipped(new BpuUpdateIO))
  val tlb    = IO(new TLBSearchIO)
  val if1    = Module(new FakeIF1)
  if1.io.bpuUpdateIn <> update
  if1.io.tlb <> tlb
  if1.io.in.npc         := if1.io.out.bits.predictResult(0).target | if1.io.out.bits.predictResult(1).target
  if1.io.in.flush       := false.B
  if1.io.in.isDelaySlot := if1.io.out.bits.validMask.asUInt.orR
  if1.io.out.ready      := true.B
}

class WrapBTB extends MycpuModule {
  val update = IO(Flipped(new BpuUpdateIO))
  val btb    = Module(new BranchTargetBuffer)
  (0 until 4).foreach(i => {
    btb.readAddr(i) := btb.readRes(i).target & btb.readRes(i).instType.asUInt
  })
  btb.update.pc   := update.pc
  btb.update.data := update.btb
}

object SubMain extends App {

  def top       = new WrapBTB
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}
