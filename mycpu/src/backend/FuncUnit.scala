package backend

import config._
import chisel3.Flipped
import chisel3.util.Decoupled
import chisel3._
import bundle._
import chisel3.util.Valid
import utils._

//read prf in "Backend" use FU.in.
//pipelineCnnect rs.out-fu.in
//io.out.wprf can use as bypass
class FuncUnit(kind: FuType.t) extends MycpuModule {
  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(new RsOutIO(kind)))
    val out   = Decoupled(new FunctionUnitOutIO)
    val flush = Input(Bool())

    val datasFromPrf = Vec(srcDataNum, Input(UInt(dataWidth.W)))
    //valid is pipex_valid
    val bypassIn =
      if (FuType.needByPassIn(kind)) Some(Flipped(Valid(new WPrfBundle))) else None
  })
  val roStage = Module(new RoStage(fuKind = kind))
  roStage.io.in <> io.in
  asg(roStage.io.datasFromPrf, io.datasFromPrf)
  if (FuType.needByPassIn(kind)) { asg(roStage.io.datasFromBypass.get, io.bypassIn.get) }
}
