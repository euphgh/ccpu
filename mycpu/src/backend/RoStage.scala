package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils._
import utils.BytesWordUtils._
import frontend.RATWriteBackIO
import backend.mem.IndexPredictor
import difftest.DifftestMemIndex

/**
  * prfData is read in "Backend",connect to io.in
  * instantiate stage in alu/mdu/lsu
  *
  * select src in this stage
  * Bypass:
  *      前面的x条来自其他FU
  *      最后一条来自本FU(如果需要)
  */

class RoStage(fuKind: FuType.t) extends MycpuModule {
  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(new RsRealOutIO(fuKind)))
    val out   = Decoupled(new ReadOpStageOutIO(fuKind))
    val flush = Input(Bool())

    val datasFromPrf = Vec(srcDataNum, Input(UInt(dataWidth.W)))
    val datasFromBypass =
      if (FuType.needBpIn(fuKind)) Some(Flipped(Vec(FuType.bpNum(fuKind), Valid(new WPrfBundle)))) else None

    val wSrat = Valid(new RATWriteBackIO) //only for malu and salu
  })

  //simple connect
  val outBits  = io.out.bits
  val inBits   = io.in.bits
  val inOrigin = inBits.origin
  val inBasic  = inOrigin.basic
  val inUop    = inOrigin.uOp
  asg(outBits.uOp, inOrigin.uOp)
  asg(outBits.destPregAddr, inBasic.destPregAddr)
  asg(outBits.destAregAddr, inBasic.destAregAddr)
  asg(outBits.exDetect, inBasic.exDetect)
  asg(outBits.robIndex, inBasic.robIndex)
  if (debug) asg(outBits.debugPC.get, inBasic.debugPC.get)

  //处理readyGo
  val pSrcs       = inBasic.pSrcs
  val inPrf       = inBits.inPrf
  val inMayNeedBp = inBits.mayNeedBp
  val byPassV     = WireInit(VecInit.fill(srcDataNum)(false.B))
  val byPassVReg  = RegInit(VecInit.fill(srcDataNum)(false.B))
  (0 until srcDataNum).map(i => {
    when(io.in.valid && !io.out.fire) { byPassVReg(i) := byPassV(i) | byPassVReg(i) }
    when(io.flush || io.out.fire) { byPassVReg(i) := false.B }
  })
  val dataRdy = WireInit(
    VecInit((0 until srcDataNum).map(i => (!inMayNeedBp(i) | byPassV(i) | byPassVReg(i) | inPrf(i))))
  ).asUInt.andR
  val notDelayRead = WireInit(true.B) //MALU和LSU会对它赋值
  val readyGo      = dataRdy && notDelayRead
  io.out.valid := io.in.valid && readyGo
  io.in.ready  := !io.in.valid || io.out.ready && readyGo

  /**
    * select srcs:
    *   default:read from prf,num_0 will get 0
    *   mdu:
    *     mfc0/mtc0：c0Addr -> src1(7,0)
    *   lsu:
    *     bypass
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
  asg(outBits.prevData, io.datasFromPrf(0)) //read prevdata as src0

  //Bypass
  if (FuType.needBpIn(fuKind)) {
    val bypass = io.datasFromBypass.get
    bypass.foreach(bp =>
      List.tabulate(srcDataNum)(i => {
        when(bp.valid && bp.bits.pDest === pSrcs(i) && pSrcs(i).orR) {
          asg(outSrcs(i), bp.bits.result)
          asg(byPassV(i), true.B)
        }
      })
    )
  }

  //alu special
  if (fuKind == FuType.MainAlu || fuKind == FuType.SubAlu) {
    val aluType = inUop.aluType.get
    //subAlu：(imm as src2)|(sa as src1)
    if (fuKind == FuType.SubAlu) {
      val imm = inOrigin.immOffset.get
      when(AluType.useImm(aluType)) {
        asg(outSrcs(1), Mux(AluType.zeroExt(aluType), ZeroExt(imm, 32), SignExt(imm, 32)))
      }
      when(AluType.needSa(aluType)) { asg(outSrcs(0), ZeroExt(imm(10, 6), 32)) }
    }
    //mainAlu：(imm as src2)|(sa as src1)|(linkAddr as src2)|(extra take)
    if (fuKind == FuType.MainAlu) {
      val maExtraIn = inOrigin.mAluExtra.get
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

      //movzn
      val blkMaluRo = Wire(Bool())
      BoringUtils.addSink(blkMaluRo, "blockMaluRo")
      asg(notDelayRead, !blkMaluRo)
      val src0Reg  = RegNext(outSrcs(0))
      val src1Reg  = RegNext(outSrcs(1))
      val movznReg = RegNext(blkMaluRo)
      when(movznReg) {
        outSrcs(0) := src0Reg
        outSrcs(1) := src1Reg
      }
    }
  }

  //mdu special
  if (fuKind == FuType.Mdu) {
    val op = inUop.mduType.get
    when(MduType.isC0Inst(op)) {
      asg(outSrcs(1), ZeroExt(inOrigin.c0Addr.get, 32))
    }
  }

  //lsu special
  if (fuKind == FuType.Lsu) {
    import MemType._
    val outMem    = outBits.mem.get
    val addrL12sb = inOrigin.immOffset.get(11, 0) +& outSrcs(0)(11, 0)
    outMem.immOffset := inOrigin.immOffset.get
    outMem.carryOut  := addrL12sb(12)
    val aboveLowBound = outSrcs(0)(28, 15).orR
    val belowUppBound = !outSrcs(0)(28, 15).andR
    val mustInSeg     = aboveLowBound && belowUppBound

    // DCache Index Predictor ===================================
    val mipWIO = Wire(Flipped(Valid(new IndexPredictor.WriteIO)))
    BoringUtils.addSink(mipWIO, "MIP_WRITE_IO")
    val mip = Module(new IndexPredictor)
    mip.io.readReq := inOrigin.pcVal.get
    mip.io.writeReq <> mipWIO
    val mipRes = mip.io.readRes
    asg(outMem.mipOut, mipRes)
    // val toCacheIdx = outSrcs(0)(11, DcacheOffsetWidth)
    val toCacheIdx = Mux(mipRes.valid, mipRes.bits.idx, outSrcs(0)(11, DcacheOffsetWidth))
    // val toCacheIdx = Mux(mipRes.valid && mipRes.bits.cnt > 1.U, mipRes.bits.idx, outSrcs(0)(11, DcacheOffsetWidth))
    outMem.pcVal := inOrigin.pcVal.get

    // in perf and func, no tlb, move dir in here
    import cop.{config0, status}
    val statusReg  = Wire(new status)
    val config0Reg = Wire(new config0)
    BoringUtils.addSink(statusReg, "status")
    BoringUtils.addSink(config0Reg, "config0")
    val erl = Wire(Bool())
    val k0  = config0Reg.k0
    asg(erl, statusReg.erl)
    val isDir = outSrcs(0)(31, 30) === "b10".U || erl
    val cattr = Mux(outSrcs(0)(29), CCAttr.Uncached, CCAttr.safe(k0)._1)
    outMem.isDir    := isDir && mustInSeg
    outMem.dirCattr := cattr

    outMem.rLowAddr.offset := addrL12sb(DcacheOffsetWidth - 1, 0)
    outMem.rLowAddr.index  := addrL12sb(11, DcacheOffsetWidth)

    if (verilator) {
      val diffMemIdx = Module(new DifftestMemIndex)
      diffMemIdx.io.clock := clock
      diffMemIdx.io.en    := io.out.fire || mip.io.writeReq.valid
      // read
      diffMemIdx.io.realIdx := outMem.rLowAddr.index
      diffMemIdx.io.predIdx := toCacheIdx
      diffMemIdx.io.readIdx := mip.io.readRes.bits.idx
      diffMemIdx.io.pc      := inOrigin.pcVal.get
      diffMemIdx.io.find    := mip.io.readRes.valid
      diffMemIdx.io.cnt     := mip.io.readRes.bits.cnt
      diffMemIdx.io.ren     := io.out.fire
      // write
      diffMemIdx.io.writeWen := mip.io.writeReq.valid
      diffMemIdx.io.wPC      := mip.io.writeReq.bits.pc
      diffMemIdx.io.wIndex   := mip.io.writeReq.bits.wData.idx
      diffMemIdx.io.wCnt     := mip.io.writeReq.bits.wData.cnt
      diffMemIdx.io.idxMatch := mip.io.writeReq.bits.idxMatch
      diffMemIdx.io.tagMatch := mip.io.writeReq.bits.tagMatch
    }

    outMem.cache.rwReq.get.lowAddr.offset := addrL12sb(DcacheOffsetWidth - 1, 0)
    outMem.cache.rwReq.get.lowAddr.index  := toCacheIdx
    outMem.cache.rwReq.get.isWrite        := inOrigin.uOp.memType.get.isOneOf(SB, SH, SW, SWL, SWR, SC)
    outMem.cache.rwReq.get.wWord          := outSrcs(1)
    outMem.cache.rwReq.get.size           := DontCare
    outMem.cache.rwReq.get.wStrb          := DontCare
    if (enableCacheInst) {
      outMem.cache.cacheInst.get.valid      := inOrigin.uOp.memType.get.isOneOf(CACHEINST)
      outMem.cache.cacheInst.get.bits.op    := inOrigin.cacheOp.get
      outMem.cache.cacheInst.get.bits.taglo := 0.U
    }
    //lwl lwr
    val blkLsuRo = Wire(Bool())
    BoringUtils.addSink(blkLsuRo, "blockLsuRo")
    asg(notDelayRead, !blkLsuRo)
    val src0Reg = RegNext(outSrcs(0))
    val src1Reg = RegNext(outSrcs(1))
    val lwlrReg = RegNext(blkLsuRo)
    when(lwlrReg) {
      outSrcs(0) := src0Reg
      outSrcs(1) := src1Reg
    }
  }

  //wake up others
  val wakeUpSource = Wire(Valid(PRegIdx))
  asg(wakeUpSource.bits, outBits.destPregAddr)
  asg(wakeUpSource.valid, io.out.fire)
  // if (fuKind == FuType.MainAlu) {
  //   BoringUtils.addSource(wakeUpSource, "mAluRoWakeUp")
  // }
  // if (fuKind == FuType.SubAlu) {
  //   BoringUtils.addSource(wakeUpSource, "sAluRoWakeUp")
  // }

  //wsrat
  //only malu and salu wsrat wen here
  val wSratB = io.wSrat.bits
  wSratB.aDest   := outBits.destAregAddr
  wSratB.pDest   := outBits.destPregAddr
  io.wSrat.valid := false.B //default
  if (fuKind == FuType.MainAlu || fuKind == FuType.SubAlu) {
    io.wSrat.valid := io.out.fire
  }
}
