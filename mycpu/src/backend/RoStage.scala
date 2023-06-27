package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.asg
import utils._

/**
  * prfData is read in "Backend",connect to io.in
  * instantiate stage in alu/mdu/lsu
  *
  * select src in this stage
  * since we only care about the rob num in the bypass,we do not need to name bypass:
  *        (0) is for extern
  *        (1) is for inside
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

  //simple connect
  asg(io.out.bits.decoded, io.in.bits.basic.decoded)
  asg(io.out.bits.destPregAddr, io.in.bits.basic.destPregAddr)
  asg(io.out.bits.destAregAddr, io.in.bits.basic.destAregAddr)
  asg(io.out.bits.exception, io.in.bits.basic.exception)
  asg(io.out.bits.robIndex, io.in.bits.basic.robIndex)

  /**
    * select srcs:
    *   default:read from prf
    *     num_0 will get 0,dontcare
    *   mAlu/sAlu:
    *     should consider bypass
    *     I-inst(RS):src2 should be imm(这里ldst的src2不用被选成imm)
    *     SLL/SRA/SRL(RSRT):src1 should be sa
    *   mAlu:"AL"(RS) should take link addr in src2
    */
  val outSrcs = io.out.bits.srcData
  (0 until srcDataNum).map(i => asg(outSrcs(i), io.datasFromPrf(i))) //default
  if (fuKind == FuType.MainAlu || fuKind == FuType.SubAlu) {
    //bypass
    val bypass = io.datasFromBypass.get
    val pSrcs  = io.in.bits.basic.srcPregs
    List.tabulate(srcDataNum)(i =>
      List.tabulate(aluBypassNum)(j =>
        when(bypass(j).valid && bypass(j).bits.pDest === pSrcs(i).pIdx && pSrcs(i).pIdx =/= 0.U) {
          asg(outSrcs(i), bypass(j).bits.result)
        }
      )
    )
    //select (imm as src2)|(sa as src1)
    val srcType = io.in.bits.basic.decoded.srcType
    val aluType = io.in.bits.basic.decoded.aluType
    val needSa  = AluType.isSll(aluType) || AluType.isSra(aluType) || AluType.isSrl(aluType)
    if (fuKind == FuType.SubAlu) {
      val imm = io.in.bits.immOffset.get
      when(srcType === SRCType.RS) { asg(outSrcs(1), imm) }
      when(needSa) { asg(outSrcs(0), imm(10, 6)) }
    }
    if (fuKind == FuType.MainAlu) {
      val maExtraIn = io.in.bits.mAluExtra.get
      val low26     = maExtraIn.low26
      val imm       = low26(15, 0)
      //count target
      val brType = io.in.bits.basic.decoded.brType
      val isJr   = BranchType.isJr(brType)
      val isJ    = BranchType.isJ(brType)
      val isAl   = BranchType.isAL(brType)
      val bDest  = SignExt(Cat(imm, 0.U(2.W)), 32)
      val jDest  = Cat(maExtraIn.dsPcVal(31, 28), low26, 0.U(2.W))
      val jrDest = outSrcs(0)
      //select target,connect preRes
      val outBranch = io.out.bits.branch.get
      asg(outBranch.realTarget, Mux(isJ, jDest, Mux(isJr, jrDest, bDest)))
      asg(outBranch.predictResult, maExtraIn.predictResult)
      //srcs
      when(srcType === SRCType.RS) { asg(outSrcs(1), imm) }
      when(needSa) { asg(outSrcs(0), imm(10, 6)) }
      when(isAl) { asg(outSrcs(1), maExtraIn.dsPcVal + 4.U) }
    }
  }

  /**
    */
  if (fuKind == FuType.Mdu) { asg(io.out.bits.mfc0Addr.get, io.in.bits.mfc0Addr.get) }

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
    val outMem    = io.out.bits.mem.get
    val addrL12sb = io.in.bits.immOffset.get(11, 0) +& outSrcs(0)(11, 0)
    outMem.cache.rwReq.get.lowAddr.offset := addrL12sb(cacheOffsetWidth - 1, 0)
    outMem.cache.rwReq.get.lowAddr.index  := addrL12sb(11, cacheOffsetWidth)
    outMem.immOffset                      := io.in.bits.immOffset.get
    outMem.carryout                       := addrL12sb(12)
  }
}
