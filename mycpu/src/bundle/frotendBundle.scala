package bundle

import chisel3._
import chisel3.util._
import config._
import cache._
import decodemacro.MacroDecode

/*==================== SOME BASIC BUNDLE ====================*/

//decoupled with badVaddr
class ExceptionInfoBundle extends MycpuBundle {
  val happen  = Output(Bool())
  val isBd    = Output(Bool())
  val excCode = Output(ExcCode())
  val pc      = Output(UWord)
  val refill  = Output(Bool())
}

//bpu info for per inst
class PredictResultBundle extends MycpuBundle {
  val counter = UInt(2.W)
  val brType  = BtbType()
  val target  = UInt(vaddrWidth.W)
}

class BasicInstInfoBundle extends MycpuBundle {
  val instr = Output(UInt(instrWidth.W))
  val pcVal = Output(UInt(vaddrWidth.W))
}

/**
  * blkType ->  [Rs]  lsu/mdu
  * brType  ->  [Rs][Ro][Exe]  mAlu
  * aluType ->  [Rs][RO][Exe]  mAlu sAlu
  * memType ->  [Rs][Ro][Mem1][Mem2] lsu
  * mduType ->  [Rs][Ro][Exe]mdu
  * specialType -> ROB
  */
@MacroDecode
class DecodeInstInfoBundle extends MycpuBundle {
  val blockType   = BlockType() //TODO:不要了
  val specialType = SpecialType() //带有Non，ROB里啥都有
  val brType      = BranchType() //带有Non，因为mALU里不止走branch
  val aluType     = AluType() //带有Non，因为mAlu里不止走aluInst
  val memType     = MemType() //不带Non
  val mduType     = MduType() //不带Non
}

//uOps是给FU流水级用的
//直接写到RsOutIO和RoOut中
class UOps(kind: FuType.t) extends MycpuBundle {
  val brType  = if (kind == FuType.MainAlu) Some(Output(BranchType())) else None
  val aluType = if (kind == FuType.MainAlu || kind == FuType.SubAlu) Some(Output(AluType())) else None
  val memType = if (kind == FuType.Lsu) Some(Output(MemType())) else None
  val mduType = if (kind == FuType.Mdu) Some(Output(MduType())) else None
}

//no need a wen,pDest===0 means !wen
class WPrfBundle extends MycpuBundle {
  val pDest  = PRegIdx
  val result = UWord
  val wmask  = UInt(4.W)
}

class WbRobBundle extends MycpuBundle {
  val robIndex     = Output(UInt(robIndexWidth.W))
  val exception    = new ExceptionInfoBundle
  val isMispredict = Output(Bool())
}

/*==================== 流水级OUT接口，不带valid-rdy ====================*/

class PreIfOutIO extends MycpuBundle {
  val npc         = Output(UInt(vaddrWidth.W))
  val isDelaySlot = Output(Bool()) // tell stage1 alignMask should be b1000
  val flush       = Output(Bool())
}

//should be fast, because in one cycle
class IfStage1ToPreIf extends MycpuBundle {
  val stage1Rdy  = Output(Bool())
  val pcVal      = Output(UInt(vaddrWidth.W))
  val dsFetched  = Output(Bool())
  val hasBranch  = Output(Bool())
  val predictDst = Output(UWord)
}

//can be slow, register will stage them
class IfStage1OutIO extends MycpuBundle {
  val validMask      = Output(Vec(fetchNum, Bool()))
  val pcVal          = Output(UInt(vaddrWidth.W))
  val tagOfInstGroup = Output(UInt(tagWidth.W))
  val isUncached     = Output(Bool())
  val exception      = Output(FrontExcCode())
  val iCache         = new CacheStage1OutIO(IcachRoads, 8, false)
  val predictResult  = Output(Vec(fetchNum, new PredictResultBundle))
}

class IfStage2OutIO extends MycpuBundle {
  val predictResult = Vec(fetchNum, new PredictResultBundle)
  val basicInstInfo = Vec(fetchNum, new BasicInstInfoBundle)
  val validMask     = Vec(fetchNum, Bool())
  val exception     = Output(FrontExcCode())
}

class InstARegsIdxBundle extends MycpuBundle {
  val (src0, src1, dest) = (ARegIdx, ARegIdx, ARegIdx)
}
class InstBufferOutIO extends MycpuBundle {
  val basic         = new BasicInstInfoBundle
  val predictResult = new PredictResultBundle
  val exception     = FrontExcCode()
  val whichFu       = ChiselFuType()
  val aRegsIdx      = new InstARegsIdxBundle
}

class SRATEntry extends MycpuBundle {
  val pIdx  = Output(PRegIdx)
  val inPrf = Output(Bool())
}

//rsBasicEntry < rsOutIO(each rs may has extra)
class RsBasicEntry extends MycpuBundle {
  val exception    = new ExceptionInfoBundle
  val decoded      = new DecodeInstInfoBundle //TODO:delete this and fix other place
  val destAregAddr = Output(ARegIdx)
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val srcPregs     = Vec(srcDataNum, new SRATEntry)
  val robIndex     = Output(ROBIdx)
}

/**
  * Lsu/sAlu extra:imm
  *   sAlu:may select as src2
  *   ldst:count addr(take to mem1)
  *
  * mAlu extra:
  *   branch:dsPC,low26,predictRes
  *     predictRes take to EXE
  *     dsPc and low26 count a target,take to EXE
  *     for "AL" branch,we notice that it don't need src2
  *       so we can take the link addr in src2
  *   notice that for I-inst,it should take low16 bit of low26 as its src2
  */
class RsOutIO(kind: FuType.t) extends MycpuBundle {
  val basic = new RsBasicEntry
  //val decoded=new(DecodeInstInfoBundle(kind))TODO:
  val immOffset = if (kind == FuType.Lsu || kind == FuType.SubAlu) Some(Output(UInt(immWidth.W))) else None
  val c0Addr    = if (kind == FuType.Mdu) Some(Output(CP0Idx)) else None
  val mAluExtra =
    if (kind == FuType.MainAlu) Some(new Bundle {
      val dsPcVal       = Output(UWord)
      val low26         = Output(UInt(26.W)) //携带有immoffset
      val predictResult = new PredictResultBundle
    })
    else None
}
class DispatchToRobBundle extends MycpuBundle {
  val pc          = UWord // difftest check execution flow
  val prevPDest   = PRegIdx // free when retire
  val currPDest   = PRegIdx // updata A-RAT when retire
  val currADest   = ARegIdx // updata A-RAT when retire
  val specialType = SpecialType()
}

/**
  * to rob
  *  robIndex
  *  exception
  *  isMispredict
  *  memReqVaddr
  * to prf
  *   dest
  *   wen
  *   data(alu/mdu:cal load:load store:DontCare)
  *   wmask:only load care
  * to srat
  *   destAregAddr
  *   destPregAddr
  */
class FunctionUnitOutIO extends MycpuBundle {
  val wbRob        = new WbRobBundle
  val wPrf         = new WPrfBundle
  val destAregAddr = Output(ARegIdx)
}

/**
  * exception for wbRob
  * robIndex for wbRob
  * destPregAddr is for Wprf/srat
  * destAregAddr is for srat
  *
  * srcDatas:=mux(pregData,Bypass)
  *
  * the mem is for lsu
  *   index/offset:12 bit cal
  *   memType:take from decode
  *   wWord:load dont care/store read src
  *   size:gen in Rostage
  *   wStrb:load dont care/actually store dont care at this stage
  *
  * the branch is for branch in mAlu
  *
  * special:
  *   link addr->src2
  *   imm->src2
  *   sa->src1
  *   {0.U(24.W),c0Addr}->src1
  */
class ReadOpStageOutIO(kind: FuType.t) extends MycpuBundle {
  val robIndex     = Output(UInt(robIndexWidth.W))
  val exception    = new ExceptionInfoBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val destAregAddr = Output(ARegIdx)
  val decoded      = new DecodeInstInfoBundle //TODO:Uops

  val srcData = Vec(2, Output(UInt(dataWidth.W)))

  val branch =
    if (kind == FuType.MainAlu) Some(new Bundle {
      val realTarget    = Output(UWord)
      val predictResult = new PredictResultBundle
    })
    else None
  val mem =
    if (kind == FuType.Lsu) Some(Output(new Bundle {
      val cache     = Output(new CacheStage1In(true)) //cache.rwReq.wWord is just src2?
      val immOffset = Output(UInt(16.W))
      val carryout  = Output(Bool())
    }))
    else None
}

//just use to instantiate exeStageIO in alu/mdu
class ExeStageIO(fuKind: FuType.t) extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuKind)))
  val out = Decoupled(new FunctionUnitOutIO)
}
