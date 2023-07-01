// import Mill dependency
import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import mill.scalalib.TestModule.ScalaTest
// support BSP
import mill.bsp._

trait ChiselModule extends ScalaModule with ScalafmtModule {
  override def scalaVersion = "2.13.10"
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.6.0"
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  )
}

object submacro extends ChiselModule {
  override def scalacOptions = Seq(
    "-unchecked",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-Ymacro-annotations",
    "-Xfatal-warnings"
    // "-Ywarn-dead-code",
    // "-Ywarn-unused",
    // "-Ymacro-annotations"
  )
}

object mycpu extends ChiselModule { m =>
  def moduleDeps = super.moduleDeps ++ Seq(submacro)
  override def scalacOptions = Seq(
    "-unchecked",
    "-language:reflectiveCalls",
    "-deprecation",
    "-feature",
    "-Xcheckinit",
    "-Ymacro-annotations",
    "-Xfatal-warnings"
    // "-Ymacro-debug-lite"
    // "-Ywarn-dead-code",
    // "-Ywarn-unused",
    // "-Ymacro-annotations"
  )
  object test extends Tests with ScalaTest {
    override def ivyDeps = m.ivyDeps() ++ Agg(
      ivy"edu.berkeley.cs::chiseltest:0.6.0"
    )
  }
}
