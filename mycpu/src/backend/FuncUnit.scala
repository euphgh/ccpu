package backend

import config._
import chisel3.Flipped
import chisel3.util.Decoupled
import chisel3._
import bundle._

class FuncUnit(rsKind: FuType.t) extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new RsOutIO(rsKind)))
    val datasFromBypass =
      if (FuType.needByPass(rsKind)) Some(Vec(aluBypassNum, Flipped(new WPrfBundle))) else None
    val out = Decoupled(new FunctionUnitOutIO)
  })
  val roStage = Module(new RoStage(fuKind = FuType.MainAlu))
  roStage.io.in <> io.in
}
