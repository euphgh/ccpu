/* only define a class if it will be  reused*/

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
  //type
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
  val srcAregAddrs = Vec(srcDataNum, Output(UInt(aRegAddrWidth.W)))
  val destAregAddr = Output(UInt(aRegAddrWidth.W))
}

//we care about "Rdy" of srcs
class SrcPregsBundle extends MycpuBundle {
  val addr    = Output(UInt(pRegAddrWidth.W))
  val dataRdy = Output(Bool())
}
//use 2 src aRegAddr to get their pregAddr from s-rat
//get destPregAddr from freeList
//use destPregAddr to get  prevDestPregAddr from s-rat
class RenameInfoBundle extends MycpuBundle {
  val srcPregs         = Vec(srcDataNum, new SrcPregsBundle)
  val destPregAddr     = Output(UInt(pRegAddrWidth.W))
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
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
  val exception        = new ExceptionInfoBundle
  val nextTarget       = Output(UInt(vaddrWidth.W))
  val flushBackend     = Output(Bool())
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W)) //to freeList
  val destAregAddr     = Output(UInt(aRegAddrWidth.W)) //to a-rat
  val destPregAddr     = Output(UInt(pRegAddrWidth.W)) //to a-rat
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
  val npc   = Output(UInt(vaddrWidth.W))
  val flush = Output(Bool())
}
class IfStage1OutIO extends MycpuBundle {
  val pcVal          = Output(UInt(vaddrWidth.W))
  val bpuOut         = new BpuOutIO
  val alignMask      = Output(UInt(fetchNum.W))
  val tagOfInstGroup = Output(UInt(tagWidth.W))
  val exception      = Output(FrontExcCode())
  val iCache         = Output(new CacheStage1OutIO(IcachRoads, false))
}

//TODO:may declare a bundle for basic/predictres/exception (name what?)
class IfStage2OutIO extends MycpuBundle {
  val predictResult = Vec(fetchNum, new PredictResultBundle)
  val basicInstInfo = Vec(fetchNum, new BasicInstInfoBundle)
  val validNum      = Output(UInt(log2Up(fetchNum).W))
  val exception     = Output(FrontExcCode())
}
class DecodeStageOutIO extends MycpuBundle {
  val predictResult = new PredictResultBundle
  val basic         = new BasicInstInfoBundle
  val decoded       = new DecodeInstInfoBundle
  val exception     = new ExceptionInfoBundle
}

class DispatchToRobBundle extends MycpuBundle {
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
  val destAregAddr     = Output(UInt(aRegAddrWidth.W))
  val destPregAddr     = Output(UInt(pRegAddrWidth.W))
  val basic            = new BasicInstInfoBundle
}

/**
  * rsBasicEntry < rsOutIO(also use as InIO,may with pre) < rsEntry(with Valid)
  * dispatcher.io.toRs = rsBasicEntry + pre
  *
  * in "Backend" , match 3 dispatcher.io.toRs to different Rs.in
  *   according to inst type
  *   take care of predictRes
  */
class RsBasicEntry extends MycpuBundle {
  val exception = new ExceptionInfoBundle
  val decoded   = new DecodeInstInfoBundle

  val srcPregs     = Vec(srcDataNum, new SrcPregsBundle) //TODO:change to sratEntry
  val destPregAddr = Output(UInt(pRegAddrWidth.W))

  val robIndex = Output(UInt(robIndexWidth.W))
}
class RsOutIO(kind: Int) extends RsBasicEntry {
  //val predictResult = if (kind == fuType.Alu.id) Some(new PredictResultBundle) else None
  if (kind == fuType.Alu.id) {
    val predictResult = new PredictResultBundle
  }
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
  val destAregAddr = Output(UInt(aRegAddrWidth.W))
}

/**
  * TODO:put lsu inst.offset in decoded
  * srcDatas means data read from pReg
  *
  * wbRob not change
  * destPregAddr is for Wprf
  *
  * the index and offset is for lsu
  */
class ReadOpStageOutIO(kind: Int) extends MycpuBundle {
  val wbRob        = new WbRobBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
  val decoded      = new DecodeInstInfoBundle

  val srcDatas    = Vec(2, Output(UInt(dataWidth.W)))
  val cacheIndex  = if (kind == fuType.Lsu.id) Some(Output(UInt(cacheIndexWidth.W))) else None
  val cacheOffset = if (kind == fuType.Lsu.id) Some(Output(UInt(cacheOffsetWidth.W))) else None
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

  val dCache = Output(new CacheStage1OutIO(DcachRoads, true))
}

class StoreQueueOutIO extends MycpuBundle {
  //val
}

//------------------------------------------------------------------------------------------------------
/* 将各流水级的OUT接口实例化在流水级接口里，此时带decoupled和flipped */

//instantiate "inst Buffer" in this stage!(unless the "Ibf" is decoupled from "ID" )
//write fetchNum insts in instBuffer at one cycle
//move the headPtr according to the "validNum"
//decode the dequeue insts(combination logic) at current cycle
//TODO:or we can decode them in next cycle,then the "Ibf" is decoupled from "ID"
class DecodeStageIO extends MycpuBundle {
  val in  = Flipped(Decoupled(new IfStage2OutIO))
  val out = Vec(decodeNum, Decoupled(new DecodeStageOutIO))
}

//instantiate "sRAT" in this stage!
//pop from freelist:dest
//read from s-rat :2 srcs,1 prevDest
//pay attention to WAW/RAW in a instGroup!
//listen to wenPRF...

/*
predictResult->RS
decoded->RS
exception->RS
renamed
  src,dest->RS
  prevDest->ROB
basic->ROB
 */
class RenameStageIO extends MycpuBundle {
  val in = new Bundle {
    val fromDecodeStage = Vec(decodeNum, Flipped(Decoupled(new DecodeStageOutIO)))
    val wPrf            = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
  }
  val out = Vec(
    renameNum,
    Decoupled(new Bundle {
      val predictResult = new PredictResultBundle
      val basic         = new BasicInstInfoBundle
      val decoded       = new DecodeInstInfoBundle
      val exception     = new ExceptionInfoBundle
      val renamed       = new RenameInfoBundle
    })
  )
}

/**
  *    <dispatch decoupled IO>
  *
  * renameStage out.valid
  *   its in.valid &
  *   !block condition:put the logic here instead of in "rs.in.valid" and "rob.in.valid"
  *        mispredict happen->mispredict retire |
  *        blocked insts:read cp0 -> wait until rob empty   TODO:other
  *
  * rs in.valid:
  *   renameStage out valid
  *   correspondent rob slot empty
  *     each rs allow 1(FIXME:aluRs allow 2?)inst to dispatch in
  *   !in(0).valid -> !in(1).valid  ...
  * rs in.ready:
  *   has empty slot
  *
  * rob in.valid:
  *   renameStage out valid
  *   correspondent rs slot empty
  *   !in(0).valid -> !in(1).valid  ...
  * rob.in.ready:
  *   has empty slot
  */

/**
  *               <FU in and out IO>
  *  we have 4 function Units:
  *    each RS connect its function Unit (ALURS to 2)
  *  FU In：just RoStage IO
  *    we read prf in "Backend"
  *    Bypass:no need to decouple,connect in "backend" (wprf->dataFromBypass)
  *      intra_FU：connect wire in FU module from exeStage.out to ReadOpstage
  *      inter_FU：connect wire between FU modules
  *  FU Out:exeStageIo mem2StageIO
  *    the wb width is 3,now we dicide to block mdu_out if other 3 all produce
  *  LSU:
  *    connect wires to cache in FU module
  */

//just use to instantiate exeStageIO in alu/mdu
class ExeStageIO(fuKind: Int) extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuKind)))
  val out = Decoupled(new FunctionUnitOutIO)
}
