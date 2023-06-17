package bundle

import chisel3._
import chisel3.util._
import config._

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

class PredictResultBundle extends MycpuBundle {
  val taken  = Output(Bool())
  val brType = Output(BranchType())
  val target = Output(UInt(vaddrWidth.W))
}
class BasicInstInfoBundle extends MycpuBundle {
  val instr = Output(UInt(instrWidth.W))
  val pcVal = Output(UInt(vaddrWidth.W))
}

//TODO:need more control bits here
//note:if not need a src data, just set aRegAddr as 0
class DecodeInstInfoBundle extends MycpuBundle {
  //val srcAregAddrs = Vec(srcDataNum, Output(ARegIdx))
  //val destAregAddr = Output(ARegIdx)
  //val exception    = new (ExceptionInfoBundle)
  val isBlockInst = Output(Bool())
  val isBr        = Output(Bool())
}

//no need to declare a readBundle here
//TODO:no need a wen?use addr 0 to unable wen
class WPrfBundle extends MycpuBundle {
  val wen      = Output(Bool())
  val result   = Output(UInt(dataWidth.W))
  val destAddr = Output(UInt(pRegAddrWidth.W))
}

class WbRobBundle extends MycpuBundle {
  val robIndex     = Output(UInt(robIndexWidth.W))
  val exception    = new ExceptionInfoBundle
  val isMispredict = Output(Bool())
  val memReqVaddr  = Output(UInt(vaddrWidth.W))
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

//all the insts must take its predict result with it
//takenMask will be used to gen npc in preIF,and to gen validNum in IF2
class BpuOutIO() extends MycpuBundle {
  val predictTarget = Output(Vec(predictNum, UInt(vaddrWidth.W)))
  val takenMask     = Output(UInt(predictNum.W))
  val brType        = Output(BranchType())
}
class PreIfOutIO extends MycpuBundle {
  val npc         = Output(UInt(vaddrWidth.W))
  val isDelaySlot = Output(Bool())
  val flush       = Output(Bool())
}
class IfStage1OutIO extends MycpuBundle {
  val pcVal          = Output(UInt(vaddrWidth.W))
  val bpuOut         = new BpuOutIO
  val alignMask      = Output(UInt(fetchNum.W))
  val tagOfInstGroup = Output(UInt(tagWidth.W))
  val exception      = Output(FrontExcCode())
  val iCache         = new CacheStage1OutIO(IcachRoads, false)
}

//TODO:may declare a bundle for basic/predictres/exception (name what?)
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
  val exception     = Output(FrontExcCode())
  val whichFu       = Output(ChiselFuType())
  val aRegsIdx      = Output(new InstARegsIdxBundle)
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
  val decoded      = new DecodeInstInfoBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val srcPregs     = Vec(srcDataNum, new SRATEntry)
  val robIndex     = Output(ROBIdx)
}
class RsOutIO(kind: FuType.t) extends MycpuBundle {
  val basic         = new RsBasicEntry
  val predictResult = if (kind == FuType.MainAlu) Some(new PredictResultBundle) else None
}
class DispatchToRobBundle extends MycpuBundle {
  val pc        = UWord // difftest check execution flow
  val prevPDest = PRegIdx // free when retire
  val currPDest = PRegIdx // updata A-RAT when retire
  val currADest = ARegIdx // updata A-RAT when retire
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
  * srcDatas:=mux(pregData,Bypass)
  *
  * exception for wbRob
  * robIndex for wbRob
  * destPregAddr is for Wprf
  *
  * the dcachereq is for lsu
  *   index/offset:12 bit cal
  *   size/memType:gen in Ro Stage(decoupled from decode)
  *
  *   wWord:load dont care/store read src
  *   wStrb:load dont care/actually store dont care at this stage
  */
class ReadOpStageOutIO(kind: FuType.t) extends MycpuBundle {
  val robIndex     = Output(UInt(robIndexWidth.W))
  val exception    = new ExceptionInfoBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val decoded      = new DecodeInstInfoBundle

  val srcData       = Vec(2, Output(UInt(dataWidth.W)))
  val dCacheReq     = if (kind == FuType.Lsu.id) Some(new DcacheReq(toCacheStage = 1)) else None
  val predictResult = if (kind == FuType.MainAlu) Some(new PredictResultBundle) else None
}

/**
  * decoded is for mem2:deal with loadData
  *
  * wbRob change:exception and memReqVaddr
  * tagOfMemReqPaddr is for mem2:hitOrMiss detect
  *
  * take cache data/meta with us
  * TODO:storeData not need to take?cause after mem1,store inst get into storeQ
  */

class MemStage1OutIO extends MycpuBundle {
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val decoded      = new DecodeInstInfoBundle

  val wbRob            = new WbRobBundle
  val tagOfMemReqPaddr = Output(UInt(tagWidth.W))

  val dCache = Output(new CacheStage1OutIO(DcachRoads, isDcache = true))

}

class StoreQueueOutIO extends CacheBasicReq {

  // already has index/offset
  val tagOfMemReqPaddr = Output(UInt(tagWidth.W)) //get in mem1

  val size    = Output(UInt(3.W)) //gen in RO
  val wWord   = Output(UWord) //read in RO
  val wStrb   = Output(UInt(4.W)) //
  val memType = Output(MemType()) //gen in RO
}

//------------------------------------------------------------------------------------------------------

//just use to instantiate exeStageIO in alu/mdu
class ExeStageIO(fuKind: FuType.t) extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuKind)))
  val out = Decoupled(new FunctionUnitOutIO)
}
