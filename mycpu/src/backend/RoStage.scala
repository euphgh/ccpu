package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.asg
import utils.vassert

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
      if (FuType.needByPassIn(fuKind)) Some(Vec(aluBypassNum, Flipped(Valid(new WPrfBundle)))) else None
  })

  //注意，这里的io.in.valid已经代表着pipex_valid
  val readyGo = true.B
  io.out.valid := io.in.valid && readyGo
  io.in.ready  := !io.in.valid || io.out.ready && readyGo

  //unchange signal
  asg(io.out.bits.decoded, io.in.bits.basic.decoded)
  asg(io.out.bits.destPregAddr, io.in.bits.basic.destPregAddr)
  asg(io.out.bits.destAregAddr, io.in.bits.basic.destAregAddr)
  asg(io.out.bits.exception, io.in.bits.basic.exception)
  asg(io.out.bits.robIndex, io.in.bits.basic.robIndex)
  if (fuKind == FuType.MainAlu) { asg(io.out.bits.predictResult.get, io.in.bits.predictResult.get) }
  //TODO:immOffest
  if (fuKind == FuType.Mdu) { asg(io.out.bits.mfc0Addr.get, io.in.bits.mfc0Addr.get) }

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

  //wake up
  val wakeUpSource = Wire(Valid(PRegIdx))
  asg(wakeUpSource.bits, io.out.bits.destPregAddr)
  asg(wakeUpSource.valid, io.out.fire)
  if (fuKind == FuType.MainAlu) {
    BoringUtils.addSource(wakeUpSource, "mAluRoWakeUp")
  }
  if (fuKind == FuType.SubAlu) {
    BoringUtils.addSource(wakeUpSource, "sAluRoWakeUp")
  }

  //for lsu
  if (fuKind == FuType.Lsu) {
    val memReqVaddr12 = io.in.bits.memInstOffset.get(12, 0) + io.out.bits.srcData(0)(12, 0) //use "+"
    val addr          = memReqVaddr12(1, 0)
    val dCacheReq     = io.out.bits.dCacheReq.get
    asg(dCacheReq.wWord, io.out.bits.srcData(1))
    asg(dCacheReq.offset, memReqVaddr12(cacheOffsetWidth - 1, 0))
    asg(dCacheReq.index, memReqVaddr12(11, cacheOffsetWidth))
    asg(dCacheReq.memType.get, io.in.bits.basic.decoded.memType)
    //TODO:size/wstrb
    //use memType get size,use size and memReqVaddr12(1, 0) to get wstrb
    // wStrb:load dont care/actually store dont care at this stage
  }
}
