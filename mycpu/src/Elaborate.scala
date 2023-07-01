import circt.stage._
import backend._
import config.FuType
import cache._
import Chisel.UInt
import frontend.Dispatcher
import frontend._
import backend.components.Multiplier
object Elaborate extends App {
  def top       = new InstFetch
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args ++ Seq("--full-stacktrace"), generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}
