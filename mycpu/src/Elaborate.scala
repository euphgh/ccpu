import circt.stage._

object Elaborate extends App {
  def top       = new GCD()
  val generator = Seq(chisel3.stage.ChiselGeneratorAnnotation(() => top))
  (new circt.stage.ChiselStage).execute(args, generator)
}
