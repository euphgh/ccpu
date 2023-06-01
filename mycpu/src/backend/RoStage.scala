package backend

import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * instantiate stage in alu/mdu/lsu
  *
  * TODO:use addsource for load index to cache?
  *
  * select src in this stage
  *
  * since we only care about the rob num in the bypass,we do not need to name bypass:
  *        (0) is for extern
  *        (1) is for inside
  */

class RoStage(fuKind: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val fromRs       = new RsOutIO(kind = fuKind)
      val datasFromPrf = Vec(srcDataNum, Output(UInt(dataWidth.W)))
    }))
    val datasFromBypass =
      if (fuKind == forAlu) Some(Vec(aluBypassNum, Flipped(new WPrfBundle))) else None
    val out = Decoupled(new ReadOpStageOutIO(kind = fuKind))
  })
}
