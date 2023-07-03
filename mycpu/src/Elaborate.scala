import circt.stage._
import backend._
import config.FuType
import cache._
import Chisel.UInt
import frontend.Dispatcher
import frontend._
import backend.components.Multiplier
object Elaborate extends App {
  def top       = new CCPU
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}

object SubMain extends App {
  def top       = new BranchTargetBuffer
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}
