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
  val outBits = io.out.bits
  val inBits  = io.in.bits
  val inBasic = inBits.basic
  val inUop   = inBasic.decoded
  asg(outBits.decoded, inBasic.decoded)
  asg(outBits.destPregAddr, inBasic.destPregAddr)
  asg(outBits.destAregAddr, inBasic.destAregAddr)
  asg(outBits.exception, inBasic.exception)
  asg(outBits.robIndex, inBasic.robIndex)

  /**
    * select srcs:
    *   default:read from prf,num_0 will get 0
    *   mdu:
    *     mfc0/mtc0：c0Addr -> src1(7,0)
    *   lsu:
    *     default
    *   mAlu/sAlu:
    *     bypass
    *     I-inst(RS)：imm -> src2
    *     SLL/SRA/SRL(RSRT)：sa -> src1
    *   mAlu:
    *     "AL"(RS): link addr -> src2
    *
    * extra take:
    *     mAlu:take predictRes and realTarget
    *     lsu:take "mem" bundle
    */
  val outSrcs = outBits.srcData
  (0 until srcDataNum).map(i => asg(outSrcs(i), io.datasFromPrf(i))) //default

  //alu special
  if (fuKind == FuType.MainAlu || fuKind == FuType.SubAlu) {
    val srcType = inUop.srcType
    val aluType = inUop.aluType
    val needSa  = AluType.isSll(aluType) || AluType.isSra(aluType) || AluType.isSrl(aluType)
    //bypass
    val bypass = io.datasFromBypass.get
    val pSrcs  = inBasic.srcPregs
    List.tabulate(srcDataNum)(i =>
      List.tabulate(aluBypassNum)(j =>
        when(bypass(j).valid && bypass(j).bits.pDest === pSrcs(i).pIdx && pSrcs(i).pIdx =/= 0.U) {
          asg(outSrcs(i), bypass(j).bits.result)
        }
      )
    )
    //subAlu：(imm as src2)|(sa as src1)
    if (fuKind == FuType.SubAlu) {
      val imm = inBits.immOffset.get
      when(srcType === SRCType.RS) { asg(outSrcs(1), imm) }
      when(needSa) { asg(outSrcs(0), imm(10, 6)) }
    }
    //mainAlu：(imm as src2)|(sa as src1)|(linkAddr as src2)|(extra take)
    if (fuKind == FuType.MainAlu) {
      val maExtraIn = inBits.mAluExtra.get
      val low26     = maExtraIn.low26
      val imm       = low26(15, 0)
      //count target
      val brType            = inUop.brType
      val (isJr, isJ, isAl) = (BranchType.isJr(brType), BranchType.isJ(brType), BranchType.isAL(brType))
      val bDest             = SignExt(Cat(imm, 0.U(2.W)), 32)
      val jDest             = Cat(maExtraIn.dsPcVal(31, 28), low26, 0.U(2.W))
      val jrDest            = outSrcs(0)
      //select target,connect preRes
      val outBranch = outBits.branch.get
      asg(outBranch.realTarget, Mux(isJ, jDest, Mux(isJr, jrDest, bDest)))
      asg(outBranch.predictResult, maExtraIn.predictResult)
      //srcs：注意AL指令中也有srctype===RS的，但是它们选择的依然是link addr，所以这里的顺序不能变
      when(srcType === SRCType.RS) { asg(outSrcs(1), imm) }
      when(needSa) { asg(outSrcs(0), imm(10, 6)) }
      when(isAl) { asg(outSrcs(1), maExtraIn.dsPcVal + 4.U) }
    }
  }

  //mdu special
  if (fuKind == FuType.Mdu) {
    val op = inBasic.decoded.mduType
    when(MduType.isC0Inst(op)) {
      asg(outSrcs(0), ZeroExt(inBits.c0Addr.get, 32))
    }
  }

  //lsu special
  if (fuKind == FuType.Lsu) {
    val outMem    = outBits.mem.get
    val addrL12sb = inBits.immOffset.get(11, 0) +& outSrcs(0)(11, 0)
    outMem.cache.rwReq.get.lowAddr.offset := addrL12sb(cacheOffsetWidth - 1, 0)
    outMem.cache.rwReq.get.lowAddr.index  := addrL12sb(11, cacheOffsetWidth)
    outMem.immOffset                      := inBits.immOffset.get
    outMem.carryout                       := addrL12sb(12)
  }

  //wake up others
  val wakeUpSource = Wire(Valid(PRegIdx))
  asg(wakeUpSource.bits, outBits.destPregAddr)
  asg(wakeUpSource.valid, io.out.fire)
  if (fuKind == FuType.MainAlu) {
    BoringUtils.addSource(wakeUpSource, "mAluRoWakeUp")
  }
  if (fuKind == FuType.SubAlu) {
    BoringUtils.addSource(wakeUpSource, "sAluRoWakeUp")
  }
}
