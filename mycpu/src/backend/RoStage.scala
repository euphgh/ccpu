package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.asg
import utils.vassert

class bypassBundle extends MycpuBundle {
  val pDest  = PRegIdx
  val result = UWord
}

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

class RoStage(fuKind: FuType.t) extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new RsOutIO(fuKind)))
    val out = Decoupled(new ReadOpStageOutIO(fuKind))

    val datasFromPrf = Vec(srcDataNum, Input(UInt(dataWidth.W)))
    //valid is pipex_valid
    val datasFromBypass =
      if (FuType.needByPass(fuKind)) Some(Vec(aluBypassNum, Flipped(Valid(new bypassBundle)))) else None
  })

  //注意，这里的io.in.valid已经代表着pipex_valid
  io.out.valid := io.in.valid
  io.in.ready  := !io.in.valid || io.out.fire

  //unchange signal
  asg(io.out.bits.decoded, io.in.bits.basic.decoded)
  asg(io.out.bits.destPregAddr, io.in.bits.basic.destPregAddr)
  asg(io.out.bits.exception, io.in.bits.basic.exception)
  asg(io.out.bits.robIndex, io.in.bits.basic.robIndex)

  //select srcData
  val pSrcs = io.in.bits.basic.srcPregs
  (0 until srcDataNum).map(i => asg(io.out.bits.srcData(i), io.datasFromPrf(i))) //default
  if (fuKind == FuType.MainAlu || fuKind == FuType.SubAlu) {
    val bypass = io.datasFromBypass.get
    List.tabulate(srcDataNum)(i =>
      List.tabulate(aluBypassNum)(j =>
        when(bypass(j).valid && bypass(j).bits.pDest === pSrcs(i).pIdx) {
          asg(io.out.bits.srcData(i), bypass(j).bits.result)
        }
      )
    )
  }

}
