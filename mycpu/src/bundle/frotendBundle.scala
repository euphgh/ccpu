package bundle

import chisel3._
import chisel3.util._
import config._
import cache._

/* 一些基本的bundle，尽量做到解耦 */

/*
 * need more data to address exception
 * like bad address
0x00 Int 中断
0x04 AdEL 地址错例外（读数据或取指令）  IF2
0x05 AdES 地址错例外（写数据）
0x08 Sys 系统调用例外 ID
0x09 Bp 断点例外  ID
0x0a RI 保留指令例外  ID
0x0c Ov 算出溢出例外
 */
class ExceptionInfoBundle extends MycpuBundle {
  val happen  = Output(Bool())
  val isBd    = Output(Bool())
  val excCode = Output(UInt(excCodeWidth.W))
  val pc      = Output(UWord)
  val refill  = Output(Bool())
}

/*
frontend:pre-decode in IfStage2IO
  J:direction
  pc-relative:target
backend:exeStage
  B:direction
  indirect:target
 */
class MisPredictRedirectBundle extends MycpuBundle {
  val realTarget    = Output(UInt(vaddrWidth.W))
  val realDirection = Output(Bool())
  val valid         = Output(Bool())
  val brType        = Output(BranchType())
}

//Gen nextTarget in ROB
//when eret_flush:c0_epc
//when exception :
class ExceptionRedirectBundle extends MycpuBundle {
  val nextTarget = Output(UInt(vaddrWidth.W))
  val valid      = Output(Bool())
}

/**
  * bpu info for per inst
  */
class PredictResultBundle extends MycpuBundle {
  val counter = UInt(2.W)
  val brType  = BranchType()
  val target  = UInt(vaddrWidth.W)
}
class BasicInstInfoBundle extends MycpuBundle {
  val instr = Output(UInt(instrWidth.W))
  val pcVal = Output(UInt(vaddrWidth.W))
}

//TODO:@
class DecodeInstInfoBundle extends MycpuBundle {
  val blockType   = BlockType()
  val brType      = BranchType()
  val memType     = MemType()
  val mduType     = MduType()
  val specialType = SpecialType()
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
  val takeWord     = Output(UWord) //for ldst it's memReqVaddr,for mtxx it's wdata
}

class RetireBundle extends MycpuBundle {
  val exception        = new ExceptionInfoBundle //exception flush all
  val flushBackend     = Output(Bool()) //mispredict only flushBackend
  val destAregAddr     = Output(ARegIdx) //to a-rat
  val prevDestPregAddr = Output(PRegIdx) //to freeList
  val destPregAddr     = Output(PRegIdx) //to a-rat
}

//------------------------------------------------------------------------------------------------------
/* 各流水级的OUT接口，这些接口不带valid-rdy */

class PreIfOutIO extends MycpuBundle {
  val npc         = Output(UInt(vaddrWidth.W))
  val isDelaySlot = Output(Bool()) // tell stage1 alignMask should be b1000
  val flush       = Output(Bool())
}

/**
  * should be fast, because in one cycle
  */
class IfStage1ToPreIf extends MycpuBundle {
  val stage1Rdy  = Output(Bool())
  val pcVal      = Output(UInt(vaddrWidth.W))
  val dsFetched  = Output(Bool())
  val hasBranch  = Output(Bool())
  val predictDst = Output(UWord)
}

/**
  * can be slow, register will stage them
  */
class IfStage1OutIO extends MycpuBundle {
  val validMask      = Output(Vec(fetchNum, Bool()))
  val pcVal          = Output(UInt(vaddrWidth.W))
  val tagOfInstGroup = Output(UInt(tagWidth.W))
  val isUncached     = Output(Bool())
  val exception      = Output(FrontExcCode())
  val iCache         = new CacheStage1OutIO(IcachRoads, false)
  val predictResult  = Output(Vec(fetchNum, new PredictResultBundle))
}

class IfStage2OutIO extends MycpuBundle {
  val predictResult = Vec(fetchNum, new PredictResultBundle)
  val basicInstInfo = Vec(fetchNum, new BasicInstInfoBundle)
  val validMask     = Vec(fetchNum, Bool())
  val validNum      = Output(UInt(log2Up(fetchNum).W))
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

/**
  * not solve WAW/RAW conflict
  * only read/write data by index
  */
class SRATEntry extends MycpuBundle {
  val pIdx  = Output(PRegIdx)
  val inPrf = Output(Bool())
}

/**
  * rsBasicEntry < rsOutIO(also use as InIO,may with pre) < rsEntry(with Valid)
  * in "dispatcher" , already connect RsInPorts(4) use <rsOutIO>
  */
class RsBasicEntry extends MycpuBundle {
  val exception    = new ExceptionInfoBundle
  val decoded      = new DecodeInstInfoBundle //TODO:delete this and fix other place
  val destAregAddr = Output(ARegIdx)
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val srcPregs     = Vec(srcDataNum, new SRATEntry)
  val robIndex     = Output(ROBIdx)
}
class RsOutIO(kind: FuType.t) extends MycpuBundle {
  val basic = new RsBasicEntry
  //val decoded=new(DecodeInstInfoBundle(kind))TODO:
  val immOffset     = if (kind == FuType.Lsu) Some(Output(UInt(immWidth.W))) else None
  val mfc0Addr      = if (kind == FuType.Mdu) Some(Output(CP0Idx)) else None
  val predictResult = if (kind == FuType.MainAlu) Some(new PredictResultBundle) else None
}
class DispatchToRobBundle extends MycpuBundle {

  val pc        = UWord // difftest check execution flow
  val prevPDest = PRegIdx // free when retire
  val currPDest = PRegIdx // updata A-RAT when retire
  val currADest = ARegIdx // updata A-RAT when retire

  val specialType = SpecialType()
}

/**
  * use as alu/mdu exeStage.OutIO and mdu memStage2.OutIO!!!!
  *
  * to rob
  *  robIndex
  *  exception
  *  isMispredict
  *  memReqVaddr
  * to prf
  *   dest
  *   wen
  *   data(alu/mdu:cal load:load store:DontCare)
  * to srat
  *   destAregAddr
  *   destPregAddr
  *   TODO:combine destAregAddr and wPrf.dest as wbSrat Bundle in "Backend"
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
  * the dcachereq is for lsu
  *   index/offset:12 bit cal
  *   memType:take from decode
  *   wWord:load dont care/store read src
  *
  *   size:gen in Rostage
  *   wStrb:load dont care/actually store dont care at this stage
  */
class ReadOpStageOutIO(kind: FuType.t) extends MycpuBundle {
  val robIndex  = Output(UInt(robIndexWidth.W))
  val exception = new ExceptionInfoBundle

  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val destAregAddr = Output(ARegIdx)
  val decoded      = new DecodeInstInfoBundle

  val srcData = Vec(2, Output(UInt(dataWidth.W)))
  val mem =
    if (kind == FuType.Lsu) Some(Output(new Bundle {
      val cache     = Output(new CacheStage1In(true))
      val immOffset = Output(UInt(16.W))
      val carryout  = Output(Bool())
    }))
    else None
  val srcData       = Vec(2, Output(UInt(dataWidth.W)))
  val mfc0Addr      = if (kind == FuType.Mdu) Some(Output(CP0Idx)) else None
  val predictResult = if (kind == FuType.MainAlu) Some(new PredictResultBundle) else None
}

/**
  * wbRob
  *       change:exception and memReqVaddr
  *       robIndex keep
  *       isMispredict=DontCare
  *
  * destPregAddr is for Wprf/srat
  * destAregAddr is for srat
  *
  * decoded is for mem2:deal with loadData
  *
  * tagOfMemReqPaddr is for mem2:hitOrMiss detect
  *
  * take dCacheReq to dcache2
  */

//------------------------------------------------------------------------------------------------------

//just use to instantiate exeStageIO in alu/mdu
class ExeStageIO(fuKind: FuType.t) extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuKind)))
  val out = Decoupled(new FunctionUnitOutIO)
}
