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
    val in        = Flipped(Decoupled(new RsRealOutIO(kind)))
    val out       = Decoupled(new FunctionUnitOutIO)
    val roOutFire = Output(Bool())
    val flush     = Input(Bool())

    val datasFromPrf = Vec(srcDataNum, Input(UInt(dataWidth.W)))
    val bypassIn =
      if (FuType.needOBpIn(kind)) Some(Flipped(Vec(FuType.oBpNum(kind), Valid(new WPrfBundle)))) else None
  })
  val roStage = Module(new RoStage(fuKind = kind))
  roStage.io.in <> io.in
  roStage.io.flush := io.flush
  asg(io.roOutFire, roStage.io.out.fire)
  asg(roStage.io.datasFromPrf, io.datasFromPrf)

  //ByPass
  if (FuType.needBpIn(kind)) {
    val roBpIn = roStage.io.datasFromBypass.get
    //leading channels from other fu
    if (FuType.needOBpIn(kind)) {
      val outFuBp = io.bypassIn.get
      (0 until FuType.oBpNum(kind)).map(i => asg(roBpIn(i), outFuBp(i)))
    }
    //last channel from self fu
    if (FuType.needSBpIn(kind)) {
      val channel = FuType.bpNum(kind) - 1
      asg(roBpIn(channel).bits, io.out.bits.wPrf)
      asg(roBpIn(channel).valid, io.out.valid)
    }
  }
}
