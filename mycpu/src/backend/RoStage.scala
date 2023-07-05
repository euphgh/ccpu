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
  val inUop   = inBits.uOp
  asg(outBits.uOp, inBits.uOp)
  asg(outBits.destPregAddr, inBasic.destPregAddr)
  asg(outBits.destAregAddr, inBasic.destAregAddr)
  asg(outBits.exDetect, inBasic.exDetect)
  asg(outBits.robIndex, inBasic.robIndex)
  if (debug) asg(outBits.debugPC.get, inBasic.debugPC.get)

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
    val aluType = inUop.aluType.get
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
      when(AluType.useImm(aluType)) {
        asg(outSrcs(1), Mux(AluType.zeroExt(aluType), ZeroExt(imm, 32), SignExt(imm, 32)))
      }
      when(AluType.needSa(aluType)) { asg(outSrcs(0), ZeroExt(imm(10, 6), 32)) }
    }
    //mainAlu：(imm as src2)|(sa as src1)|(linkAddr as src2)|(extra take)
    if (fuKind == FuType.MainAlu) {
      val maExtraIn = inBits.mAluExtra.get
      val low26     = maExtraIn.low26
      val imm       = low26(15, 0)
      //count target
      val brType            = inUop.brType.get
      val dsPcVal           = maExtraIn.pcVal + 4.U(32.W)
      val (isJr, isJ, isAl) = (BranchType.isJr(brType), BranchType.isJ(brType), BranchType.isAL(brType))
      val bDest             = SignExt(Cat(imm, 0.U(2.W)), 32) + dsPcVal
      val jDest             = Cat(dsPcVal(31, 28), low26, 0.U(2.W))
      val jrDest            = outSrcs(0)
      //extra take:select target,connect preRes
      val outBranch   = outBits.branch.get
      val realBtbType = BranchType.toBtbType(inUop.brType.get, low26(25, 21)) //rs用于判断是否jret
      asg(outBranch.realBtbType, realBtbType)
      asg(outBranch.realTarget, Mux(isJ, jDest, Mux(isJr, jrDest, bDest)))
      asg(outBranch.predict, maExtraIn.predictResult)
      asg(outBranch.pcVal, maExtraIn.pcVal)
      //srcs select
      when(AluType.useImm(aluType)) {
        asg(outSrcs(1), Mux(AluType.zeroExt(aluType), ZeroExt(imm, 32), SignExt(imm, 32)))
      }
      when(AluType.needSa(aluType)) { asg(outSrcs(0), ZeroExt(imm(10, 6), 32)) }
      when(isAl) { asg(outSrcs(1), dsPcVal + 4.U) }
    }
  }

  //mdu special
  if (fuKind == FuType.Mdu) {
    val op = inUop.mduType.get
    when(MduType.isC0Inst(op)) {
      asg(outSrcs(0), ZeroExt(inBits.c0Addr.get, 32))
    }
  }

  //lsu special
  if (fuKind == FuType.Lsu) {
    import MemType._
    val outMem    = outBits.mem.get
    val addrL12sb = inBits.immOffset.get(11, 0) +& outSrcs(0)(11, 0)
    outMem.cache.rwReq.get.lowAddr.offset := addrL12sb(cacheOffsetWidth - 1, 0)
    outMem.cache.rwReq.get.lowAddr.index  := addrL12sb(11, cacheOffsetWidth)
    outMem.cache.rwReq.get.isWrite        := inBits.uOp.memType.get.isOneOf(SB, SH, SW, SWL, SWR)
    outMem.cache.rwReq.get.wWord          := outSrcs(1)
    outMem.cache.rwReq.get.size           := DontCare
    outMem.cache.rwReq.get.wStrb          := DontCare
    if (enableCacheInst) {
      outMem.cache.cacheInst.get.valid      := inBits.uOp.memType.get.isOneOf(CACHEINST)
      outMem.cache.cacheInst.get.bits.op    := inBits.cacheOp.get
      outMem.cache.cacheInst.get.bits.taglo := 0.U
    }
    outMem.immOffset := inBits.immOffset.get
    outMem.carryout  := addrL12sb(12)
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
