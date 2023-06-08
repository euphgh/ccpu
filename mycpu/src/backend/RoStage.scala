package backend

import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * prfData is read in "Backend",connect to io.in
  * instantiate stage in alu/mdu/lsu
  *
  * select src in this stage
  * since we only care about the rob num in the bypass,we do not need to name bypass:
  *        (0) is for extern
  *        (1) is for inside
  *
  * cal 12 bit index+offset for lsu(dcache1.io.in)
  */

class RoStage(fuKind: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val fromRs       = new RsOutIO(kind = fuKind)
      val datasFromPrf = Vec(srcDataNum, Output(UInt(dataWidth.W)))
    }))
    val datasFromBypass =
      if (fuKind == FuType.Alu.id) Some(Vec(aluBypassNum, Flipped(new WPrfBundle))) else None
    val out = Decoupled(new ReadOpStageOutIO(kind = fuKind))
  })

}
