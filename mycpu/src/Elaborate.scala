import circt.stage._
import java.io.FileInputStream
import java.util.Properties
import scala.jdk.CollectionConverters._
object Elaborate extends App {
  def top       = new CCPU
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new ChiselStage).execute(args, generator :+ CIRCTTargetAnnotation(CIRCTTarget.Verilog))
}
