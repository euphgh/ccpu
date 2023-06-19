// import Mill dependency
import mill._
import mill.scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import mill.scalalib.TestModule.Utest
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

object mycpu extends ScalaModule with ScalafmtModule { m =>
  override def scalaVersion = "2.13.10"
  def moduleDeps            = Seq(submacro)
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
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.6.0"
  )
  override def scalacPluginIvyDeps = Agg(
    ivy"edu.berkeley.cs:::chisel3-plugin:3.6.0"
  )
}
