/* think about when should we define a class */

package bundle

import chisel3._
import chisel3.util._

/* 一些基本的bundle，尽量做到解耦 */

/*
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
}

//Gen nextTarget in ROB
//when eret_flush:c0_epc
//when exception :bfc00380
class ExceptionRedirectBundle extends MycpuBundle {
  val nextTarget = Output(UInt(vaddrWidth.W))
  val valid      = Output(Bool())
}

class RedirectBundle extends MycpuBundle {
  val mispredict = new MisPredictBundle
  val exception  = new ExceptionRedirectBundle
}

class PredictResultBundle extends MycpuBundle {
  val direction = Output(Bool())
  val target    = Output(UInt(vaddrWidth.W))
}
class BasicInstInfoBundle extends MycpuBundle {
  val instr = Output(UInt(instrWidth.W))
  val pcVal = Output(UInt(vaddrWidth.W))
}
//need more control bits here
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

/*
use in renameStageOut
TODO:may delete this,because renameStageOutIO = InstInfoBundle
 */
class InstInfoBundle extends MycpuBundle {
  val predictResult = new PredictResultBundle
  val exception     = new ExceptionInfoBundle
  val basic         = new BasicInstInfoBundle
  val decoded       = new DecodeInstInfoBundle
  val renamed       = new RenameInfoBundle
}

//no need to declare a readBundle here
class WPrfBundle extends MycpuBundle {
  val wen      = Output(Bool())
  val destAddr = Output(UInt(pRegAddrWidth.W))
}

class BypassBundle extends MycpuBundle {
  val data     = Output(UInt(dataWidth.W))
  val robIndex = Output(UInt(robIndexWidth.W))
}

//TODO:merge these two
class RobInfoBundle extends MycpuBundle {
  val robIndex = Output(UInt(robIndexWidth.W))
}
class WbRobBundle extends MycpuBundle {
  val robInfo = new RobInfoBundle
}

class RetireBundle extends MycpuBundle {
  val redirect         = new RedirectBundle
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W)) //to freeList
}
//------------------------------------------------------------------------------------------------------
/* 各流水级的OUT接口，这些接口不带valid-rdy */

//all the insts must take its predict result with it
//takenMask will be used to gen npc in preIF,and to gen validNum in IF2
class BpuOutIO extends MycpuBundle {
  val predictTarget = Output(Vec(predictNum, UInt(vaddrWidth.W)))
  val takenMask     = Output(UInt(predictNum.W))
}
class PreIfOutIO extends MycpuBundle {
  val npc = Output(UInt(vaddrWidth.W))
}
class IfStage1OutIO extends MycpuBundle {
  val pcVal          = Output(UInt(PaddrWidth.W))
  val bpuOut         = new BpuOutIO
  val allignMask     = Output(UInt(fetchNum.W))
  val tagOfInstGroup = Output(UInt(paddrTagWidth.W))
}

class IfStage2OutIO extends MycpuBundle {
  val predictResult = Vec(fetchNum, new PredictResultBundle)
  val basicInstInfo = Vec(fetchNum, new BasicInstInfoBundle)
  val validNum      = Output(UInt(log2Up(fetchNum).W))
  val exception     = new ExceptionInfoBundel
}

class DecodeStageOutIO extends MycpuBundle {
  val predictResult = new PredictResultBundle
  val basic         = new BasicInstInfoBundle
  val decoded       = new DecodeInstInfoBundle
  val exception     = new ExceptionInfoBundel
}

class RenameStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
}

/*
  rsOut:not OK
    decoded is needed until exeStage：uOps
    renamed is needed until wbStage；use src in RoStage,use dest in wbStage
    Exception：needed until wbStage: detect in  exeStage,write in rob in wbStage
      mdu-算出溢出例外
      alu-算出溢出例外
      lsu-读数据地址错

    predictResult：only aluRs need in exeStage
    basicTODO:no need to take it into FU
 */
class RsOutIO(kind: Int) extends MycpuBundle {
  val predictResult = if (kind == forAlu) Some(new PredictResultBundle) else None
  val exception     = new ExceptionInfoBundle
  val decoded       = new DecodeInstInfoBundle
  val renamed       = new RenameInfoBundle
}

class ReadOpStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
  val robInfo  = new RobInfoBundle
  val srcDatas = Vec(2, Output(UInt(dataWidth.W)))
}

class exeStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
  val result   = Output(UInt(dataWidth.W))
}

//load inst don't care storeData
class MemStage1OutIO extends MycpuBundle {
  val instInfo         = new InstInfoBundle
  val storeData        = Output(UInt(dataWidth.W))
  val memReqVaddr      = Output(UInt(vaddrWidth.W))
  val tagOfMemReqPaddr = Output(UInt(paddrTagWidth.W))
}

//load inst don't care storeData
//store inst get into storeQ,so no need to take storeData in the FU pipeline
class MemStage2OutIO extends MycpuBundle {
  val instInfo    = new InstInfoBundle
  val memReqVaddr = Output(UInt(vaddrWidth.W))
  val loadData    = Output(UInt(dataWidth.W))
}

class FunctionUnitOutIO extends MycpuBundle {
  val exception = new ExceptionInfoBundle
  val renamed   = new RenameInfoBundle
}
class StoreQueueOutIO extends MycpuBundle {
  //val
}

//------------------------------------------------------------------------------------------------------
/* 将各流水级的OUT接口实例化在流水级接口里，此时带decoupled和flipped */

//两个输入端口TODO:无需valid-rdy？
//out:npc
//backendTarget has higherPriority:exception or misPredict
//otherWise,use takenMask and alignMask to gen nextpc
class PreIfIO extends MycpuBundle {

  val in = new Bundle {
    val fromBackend = Flipped(new RedirectBundle)
    val fromBpu     = Flipped(new BpuOutIO)
    val allignMask  = Input(UInt(fetchNum.W))
  }
  val out = Decoupled(new PreIfOutIO)
}

/* instantiate iTLB in "Frontend"
TODO:a port for tlbRead
use pc to get paddr of inst in this cycle from TLB */
//take the predictResult/pcVal/alignmask with insts
//take the tag of paddr to IF2 since we will connect it to cache in IF2
class IfStage1IO extends MycpuBundle {
  val in  = Flipped(Decoupled(new PreIfOutIO))
  val out = Decoupled(new IfStage1OutIO)
}

//use in_npc to read BTB/PHT/...
//get the predict Result in next cycle
class BpuIO extends MycpuBundle {
  val in  = Flipped(Decoupled(new PreIfOutIO))
  val out = Decoupled(new BpuOutIO)
}

//TODO:cache
//instantiate cache in frotend,connect the signal between "main pipeline" and "cache pipeline"
//do not take the META/DATA in "main pipeline"
//take then in "cache pipeline"

//pre-decode in this stage?TODO:
//connect paddrTag to cache in "Frontend",cache will resp a "hit or miss" signal
//in this stage,we gen "validNum",and use it to move headPtr of instBuffer at next cycle
class IfStage2IO extends NutCoreBundle {
  val in  = Flipped(Decoupled(new IfStage1OutIO))
  val out = Decoupled(new IfStage2OutIO)
}

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
//TODO:use addSink to gen wenPRF,instead of declare a port here!
//listen to wenPRF...
class RenameStageIO extends MycpuBundle {
  val in = new Bundle {
    val fromDecodeStage = Vec(decodeNum, Flipped(Decoupled(new DecodeStageOutIO)))
    val WPrf            = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
  }
  val out = Vec(renameNum, Decoupled(new RenameStageOutIO))
}

//next cycle：write s-rat/RS/ROB
//instantiate 3 RS 1 ROB in "backend"

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
class RsIO(rsKind: Boolean) extends MycpuBundle {

  val in = new Bundle {
    val RenameStageIn = Flipped(Decoupled(new RenameStageOutIO))
    val WPrfIn        = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
  }
  val out =
    if (rsKind == forAlu) Vec(aluFuNum, Decoupled(new RsOutIO(kind = rsKind)))
    else Decoupled(new RsOutIO(kind = rsKind))
}

//global!!
//take rob index with the insts
//no need to write an out port class for rob
class RobIO extends MycpuBundle {
  val in = new Bundle {
    val fromRenameStage = Flipped(Decoupled(new RenameStageOutIO))
    val wbRob           = Vec(wBNum, Flipped(Decoupled(new WbRobBundle)))
  }
  val out = new Bundle {
    val toFunctionUnit = Vec(issueNum, Decoupled(new RobInfoBundle))
    val toRetire       = Vec(retireNum, Decoupled(new RetireBundle))
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
    val fromRob      = Flipped(Decoupled(new RobInfoBundle))
    val datasFromPrf = Flipped(Decoupled(Vec(srcDataNum, Output(UInt(dataWidth.W)))))
    val datasFromBypass =
      if (fuKind == forAlu) Some(Vec(aluExternBypassNum, Flipped(Decoupled(new BypassBundle)))) else None
  }
}

//TODO:  use addsource for load index?
//since we only care about the rob num in the bypass,we do not need to name bypass
class ReadOpStageIO(bypassNum: Int = 0) extends MycpuBundle {

  val in = new Bundle {
    val fromRs          = Flipped(Decoupled(new InstInfoBundle))
    val fromRob         = Flipped(Decoupled(new RobInfoBundle))
    val datasFromPrf    = Flipped(Decoupled(Vec(srcDataNum, Output(UInt(dataWidth.W)))))
    val datasFromBypass = if (bypassNum > 0) Some(Vec(bypassNum, Flipped(Decoupled(new BypassBundle)))) else None
  }
  val out = Decoupled(new ReadOpStageOutIO)
}

class exeStageIO extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO))
  val out = Decoupled(new exeStageOutIO)
}

/* instantiate dTLB in "Backend"
TODO:a port for tlbRead */
// cal 32bit Vaddr here
//rise storeQEnq signal if the inst is store
//TODO: load inst wake up :use addsink?
class MemStage1IO extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO))
  val out = Decoupled(new MemStage1OutIO)
}

//TODO:cache
//instantiate dcache in backend,connect the signal between "main pipeline" and "cache pipeline"
//do not take the META/DATA in "main pipeline"
//take them in "cache pipeline"

//connect paddrTag to cache in "Backend",cache will resp a "hit or miss" signal
class MemStage2IO extends MycpuBundle {
  val in  = Flipped(Decoupled(new MemStage1OutIO))
  val out = Decoupled(new MemStage2OutIO)
}
