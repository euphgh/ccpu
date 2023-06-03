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
class RobToFuBundle extends MycpuBundle {
  val robIndex = Output(UInt(robIndexWidth.W))
}

class WbRobBundle extends MycpuBundle {
  val robIndex     = Output(UInt(robIndexWidth.W))
  val exception    = new ExceptionInfoBundle
  val isMispredict = Output(Bool())
  val memReqVaddr  = Output(UInt(vaddrWidth.W))
}

class RetireBundle extends MycpuBundle {
  val exception        = new ExceptionRedirectBundle
  val flushBackend     = Output(Bool())
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W)) //to freeList
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
  val iCache         = Output(new CacheStage1OutIO(IcachRoads, false))
}

//TODO:may declare a bundle for basic/predictres/exception (name what?)
class IfStage2OutIO extends MycpuBundle {
  val predictResult = Vec(fetchNum, new PredictResultBundle)
  val basicInstInfo = Vec(fetchNum, new BasicInstInfoBundle)
  val validMask     = Vec(fetchNum, Bool())
  val validNum      = Output(UInt(log2Up(fetchNum).W))
  val exception     = Output(FrontExcCode())
}
class DecodeStageOutIO extends MycpuBundle {
  val predictResult = new PredictResultBundle
  val basic         = new BasicInstInfoBundle
  val decoded       = new DecodeInstInfoBundle
  val exception     = new ExceptionInfoBundle
}

class InstARegsIdxIO extends MycpuBundle {
  val (src0, src1, dest) = (ARegIdx, ARegIdx, ARegIdx)
}
class InstBufferOutIO extends MycpuBundle {
  val predictResult = new PredictResultBundle
  val basic         = new BasicInstInfoBundle
  val whichFu       = Output(ChiselFuType())
  val aRegsIdx      = Output(new InstARegsIdxIO)
  val exception     = Output(FrontExcCode())
}

/*
  rsOut:not OK
    decoded is needed until exeStage：uOps
    use psrc in RoStage,use pdest in wbStage
    Exception：needed until wbStage: detect in  exeStage,write in rob in wbStage
      no exception in mdu
      alu-算出溢出例外
      lsu-读数据地址错

    predictResult：only aluRs need in exeStage
    basicTODO:no need to take it into FU
 */
class RsOutIO(kind: Int) extends MycpuBundle {
  val predictResult = if (kind == fuType.Alu.id) Some(new PredictResultBundle) else None
  val exception     = new ExceptionInfoBundle
  val decoded       = new DecodeInstInfoBundle

  val srcPregs     = Vec(srcDataNum, new SrcPregsBundle)
  val destPregAddr = Output(UInt(pRegAddrWidth.W))
}

class FunctionUnitOutIO extends MycpuBundle {
  val wbRob = new WbRobBundle
  val wPrf  = new WPrfBundle
}

/*
TODO:
  for mem readOpstage,we can use roStage.io.out.srcDatas to cal a 12 bit index in Backend */
class ReadOpStageOutIO(kind: Int) extends MycpuBundle {
  val wbRob        = new WbRobBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))

  val decoded  = new DecodeInstInfoBundle
  val srcDatas = Vec(2, Output(UInt(dataWidth.W)))
}

//decoded is for mem2:deal with loadData
//memReqVaddr is for exception(merged into wbRob)
//tagOfMemReqPaddr is for mem2:hitOrMiss detect
//store inst get into storeQ after mem1,so no need to take storeData in the FU pipeline
class MemStage1OutIO extends MycpuBundle {
  val wbRob        = new WbRobBundle
  val destPregAddr = Output(UInt(pRegAddrWidth.W))

  val decoded          = new DecodeInstInfoBundle
  val tagOfMemReqPaddr = Output(UInt(tagWidth.W))
}

class StoreQueueOutIO extends MycpuBundle {
  //val
}

//------------------------------------------------------------------------------------------------------
/* 将各流水级的OUT接口实例化在流水级接口里，此时带decoupled和flipped */

/*
select from (Rdy & HasPriority)：the selected insts will goto "ReadOpStage"
Rdy?
 1.already rdy in renameStage
 2.listen to wenPRF...next cycle the rdy bit will ↑
 3.wake-up：the selected insts broadCast its destPregAddr    TODO:just compare in the module
   inte：
     load -> otherRS (MemStage1)<2 bubble>
     alu -> otherRS (ReadOp)<1 bubble>
   intra：
     aluRS -> aluRS (when "selected")<no bubble,need bypass>
HasPriority："priorityMask" is actually a record of age
 LSU/MDU：not any older insts(in-order)
 ALU：not any rdy&older insts(ooo)
 */

/*
rsEntry
  srcPregs
  destPregAddr
  predictResult
  exception
  decoded
 */
class RsIO(rsKind: Int) extends MycpuBundle {
  val in = new Bundle {
    val fromRenameStage = Flipped(Decoupled(new Bundle {
      val srcPregs     = Vec(srcDataNum, new SrcPregsBundle)
      val destPregAddr = Output(UInt(pRegAddrWidth.W))

      val predictResult = if (rsKind == forAlu) Some(new PredictResultBundle) else None
      val exception     = new ExceptionInfoBundle
      val decoded       = new DecodeInstInfoBundle
    }))
    val wPrf = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))

  }
  val out =
    if (rsKind == forAlu) Vec(aluFuNum, Decoupled(new RsOutIO(kind = rsKind)))
    else Decoupled(new RsOutIO(kind = rsKind))
}

/*
robEntry:
  valid

  pc?(renameIn)
  instr?(renameIn)
  prevDestPregAddr(renameIn)

  memReqVaddr(wbIn)---fu produce
  exception(wbIn)---fu produce
  isMispredict-Boll(wbIn)---fu produce
 */
//RobToFuBundle is just rob index
class RobIO extends MycpuBundle {
  val in = new Bundle {
    val fromRenameStage = Flipped(Decoupled(new Bundle {
      val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
      val basic            = new BasicInstInfoBundle
    }))
    val wbRob = Vec(wBNum, Flipped(Decoupled(new WbRobBundle)))
  }
  val out = new Bundle {
    val toFunctionUnit = Vec(issueNum, Decoupled(new RobToFuBundle))
    val toRetire       = Vec(retireNum, Decoupled(new RetireBundle))
    val storeCommit    = Output(Bool())
  }
}

//rs.out ---- readPorts.addr  -----prf
//prf ------- readPorts.datas -----FU
class PrfIO extends MycpuBundle {
  val readPorts = Vec(
    prfReadPortNum,
    new Bundle {
      val addr  = Input(UInt(pRegAddrWidth.W))
      val datas = Vec(srcDataNum, Output(UInt(dataWidth.W)))
    }
  )
  val writePorts = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
}

/*
  IN AND OUT OF FUNCTION UNITS
  we have 4 function Units:
    each RS connect its function Unit (ALURS to 2 )
    ROB TODO:discuss should change to 4 ou ports!!!
    we read prf in "Backend",and connect it to FU(readOpstage)
  ALU-MDU-LSU has different PipeLine:
    instantiate stage in funcUnit PipeLine use ↓ these stageIO
  the wb width is 3,now we dicide to block mdu_out if other 3 all produce
  Bypass:
    intra_FU：connect wire in FU module from exeStage.out to ReadOpstage
    inter_FU：connect wire between FU modules
  LSU:
    connect wires to cache in FU module
 */

class FunctionUnitIO(fuKind: Int) extends MycpuBundle {
  val in = new Bundle {
    val fromRs       = Flipped(Decoupled(new RsOutIO(kind = fuKind)))
    val fromRob      = Flipped(Decoupled(new RobToFuBundle))
    val datasFromPrf = Flipped(Decoupled(Vec(srcDataNum, Output(UInt(dataWidth.W)))))
    val datasFromBypass =
      if (fuKind == forAlu) Some(Vec(aluExternBypassNum, Flipped(Decoupled(new WPrfBundle)))) else None
  }
  val out = Decoupled(new FunctionUnitOutIO)
}

//TODO:  use addsource for load index?
//select src in this stage
//since we only care about the rob num in the bypass,we do not need to name bypass
class ReadOpStageIO(fuKind: Int) extends MycpuBundle {

  val in = new Bundle {
    val fromRs       = Flipped(Decoupled(new RsOutIO(kind = fuKind)))
    val fromRob      = Flipped(Decoupled(new RobToFuBundle))
    val datasFromPrf = Flipped(Decoupled(Vec(srcDataNum, Output(UInt(dataWidth.W)))))
    val datasFromBypass =
      if (fuKind == forAlu) Some(Vec(aluExternBypassNum, Flipped(Decoupled(new WPrfBundle)))) else None
  }
  val out = Decoupled(new ReadOpStageOutIO(kind = fuKind))
}

//bypass use wprfBundle
class exeStageIO(fuKind: Int) extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuKind)))
  val out = Decoupled(new FunctionUnitOutIO)
}

/* instantiate dTLB in "Backend"
TODO:a port for tlbRead */
// cal 32bit Vaddr here
//rise storeQEnq signal if the inst is store
//TODO: load inst wake up :use addsink?
class MemStage1IO extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuType.Lsu.id)))
  val out = Decoupled(new MemStage1OutIO)
}

//TODO:cache
//instantiate dcache in backend,connect the signal between "main pipeline" and "cache pipeline"
//do not take the META/DATA in "main pipeline"
//take them in "cache pipeline"

//connect paddrTag to cache in "Backend",cache will resp a "hit or miss" signal
class MemStage2IO extends MycpuBundle {
  val in  = Flipped(Decoupled(new MemStage1OutIO))
  val out = Decoupled(new FunctionUnitOutIO)
}
