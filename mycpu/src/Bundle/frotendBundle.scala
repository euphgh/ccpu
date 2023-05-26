/* 
think about when should we define a class 
*/

package bundle

import chisel3._
import chisel3.util._

/* Bundle,用于嵌入OutIO */

//frontend:pre-decode in stage?
//backend :decodeStage ExeStage
//decoupled from exceptionRedirectIO
class MisPredictRedirectBundle extends MycpuBundle {
  val realTarget    = Output(UInt(VaddrWidth.W))
  val realDirection = Output(Bool())
  val valid         = Output(Bool())
}

//Gen nextTarget in ROB?
//when eret_flush:c0_epc
//when exception :bfc00380
class ExceptionRedirectBundle extends MycpuBundle {
  val nextTarget = Output(UInt(VaddrWidth.W))
  val valid      = Output(Bool())
}

class RedirectBundle extends MycpuBundle {
  val mispredict = new MisPredictBundle
  val exception  = new ExceptionRedirectBundle
}

class BasicInstInfoBundle extends MycpuBundle {
  val instr   = Output(UInt(instrWidth.W))
  val pcVal   = Output(UInt(vaddrWidth.W))
  val preTake = Output(Bool())
  val preDest = Output(UInt(vaddrWidth.W))
}

//note:if not need , just set addr as 0
class DecodeInstInfoBundle extends MycpuBundle {
  val srcAregAddrs = Vec(srcDataNum,Output(UInt(aRegAddrWidth.W)))
}

//TODO:change name
//we care about "Rdy" of srcs
class SrcPregsBundle extends MycpuBundle {
  val addr = Output(UInt(pRegAddrWidth.W))
  val dataRdy  = Output(Bool())
}
//use 2 src aRegAddr to get their pregAddr from s-rat
//get destPregAddr from freeList
//use destPregAddr to get  prevDestPregAddr from s-rat
class RenameInfoBundle extends MycpuBundle {
  val srcPregs     = Vec(srcDataNum, new SrcPregsBundle)
  val destPregAddr     = Output(UInt(pRegAddrWidth.W))
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
}

class InstInfoBundle extends MycpuBundle {
  val basic   = new BasicInstInfoBundle
  val decoded = new DecodeInstInfoBundle
  val renamed = new RenameInfoBundle
}

//not declare a readBundle here,because
class WPrfBundle extends MycpuBundle{
  val wen = Output(Bool())
  val destAddr     = Output(UInt(pRegAddrWidth.W))
}

//TODO:merge these
class RobInfoBundle extends MycpuBundle{
  val robIndex= Output(UInt(robIndexWidth.W))  
}
class WbRobBundle extends MycpuBundle{
  val robInfo= new RobInfoBundle
}


class RetireBundle extends MycpuBundle{
  val redirect=new RedirectBundle
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))//to freeList
}
//------------------------------------------------------------------------------------------------------
/* 各流水级的OUT接口，这些接口不带valid-rdy */


//all the insts must take its predict result with it
//takenMask will be used to gen npc in preIF,and to gen validNum in IF2
class BpuOutIO extends MycpuBundle {
  val predictTarget = Output(Vec(PredictNum, UInt(VaddrWidth.W)))
  val takenMask     = Output(UInt(PredictNum.W))
}
class PreIfOutIO extends MycpuBundle {
  val npc = Output(UInt(VaddrWidth.W))
}
class IfStage1OutIO extends MycpuBundle {
  val pcVal          = Output(UInt(PaddrWidth.W))
  val bpuOut         = new BpuOutIO
  val allignMask     = Output(UInt(fetchNum.W))
  val tagOfInstGroup = Output(UInt(PaddrTagWidth.W))
}

class IfStage2OutIO extends MycpuBundle {
  val basicInstInfo = Vec(fetchNum,new BasicInstInfoBundle)
  val validNum=Output(UInt(log2Up(fetchNum).W))
}

class DecodeStageOutIO extends MycpuBundle {
  val basicInstInfo  = new BasicInstInfoBundle
  val decodeInstInfo = new DecodeInstInfoBundle
}

class RenameStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
}



class ReadOpStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
  val robInfo  = new RobInfoBundle
  val srcDatas = Vec(2, Output(UInt(dataWidth.W)))
}

class exeStageOutIO extends MycpuBundle {
  val instInfo = new InstInfoBundle
  val srcDatas = Vec(2, Output(UInt(dataWidth.W)))
  val result   = Output(UInt(dataWidth.W))
}

class StoreQueueOutIO extends MycpuBundle{
  val 
}


//------------------------------------------------------------------------------------------------------
/* 将各流水级的OUT接口实例化在流水级接口里，此时带decoupled和flipped */

//两个输入端口TODO:无需valid-rdy？
//out:npc
//backendTarget has higherPriority:exception or misPredict
//otherWise,use takenMask and alignMask to gen nextpc
class PreIfIO extends MycpuBundle {

  val in = new Bundle 
  {
    val fromBackend = Flipped(new RedirectBundle)
    val fromBpu     = Flipped(new BpuOutIO)
    val allignMask  = Input(UInt(fetchNum.W))
  }
  val out         = Decoupled(new PreIfOutIO)
}

//take the predictResult/pcVal/alignmask with insts
//take the tag of paddr to IF2 since we will get "hit or not" result in IF2
//use pc to get paddr of inst in this cycle from TLB
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
class RenameStageIO extends MycpuBundle{

  val in = new Bundle 
  {
    val fromDecodeStage  = Vec(decodeNum, Flipped(Decoupled(new DecodeStageOutIO))) 
    val WPrf =Vec(wBNum,Flipped(Decoupled(new WPrfBundle))) 
  }
  val out = Vec(renameNum, Decoupled(new RenameStageOutIO))
}


//next cycle：write s-rat/RS/ROB
//instantiate 3 RS 1ROB in "backend"

//select from (Rdy & HasPriority)：the selected insts will goto "ReadOpStage"
//Rdy?
//--1.already rdy in renameStage
//--2.listen to wenPRF...next cycle the rdy bit will ↑         
//--3.wake-up：the selected insts broadCast its destPregAddr    TODO:just compare in the module
//----inte：
//------load -> otherRS (MemStage1)<2 bubble>
//------alu -> otherRS (ReadOp)<1 bubble>
//----intra：
//------aluRS -> aluRS (when "selected")<no bubble,need bypass>
//HasPriority："priorityMask" is actually a record of age
//--LSU/MDU：not any older insts(in-order)
//--ALU：not any rdy&older insts(ooo)                          
class RsIO(funcUnitNum: Int = 1) extends MycpuBundle {
  
  val in = new Bundle 
  {
    val RenameStageIn = Flipped(Decoupled(new RenameStageOutIO))
    val WPrfIn =Vec(wBNum,Flipped(Decoupled(new WPrfBundle)))
  }
  val out = new Bundle
  {
    val toFunctionUnit        = Vec(funcUnitNum, Decoupled(new InstInfoBundle))
  }
}

//global!!
//take rob index with the insts
class RobIO extends MycpuBundle {
  val in = new Bundle 
  {
    val fromRenameStage = Flipped(Decoupled(new RenameStageOutIO))
    val wbRob = Vec(wBNum,Flipped(Decoupled(new WbRobBundle)))
  }
  val out = new Bundle 
  {
    val toFunctionUnit = Vec(issueNum,Decoupled(new RobInfoBundle))
    val toRetire =Vec(retireNum,Decoupled(new RetireBundle))
  }
}

//rs.out ---- readPorts.addr  -----prf
//prf ------- readPorts.datas -----FU
class PrfIO extends MycpuBundle{
  val readPorts = Vec(prfReadPortNum,new Bundle
  {
    val addr = Input(UInt(pRegAddrWidth.W))
    val datas = Vec(srcDataNum,Output(UInt(dataWidth.W)))
  })
  val writePorts = Vec(wBNum,Flipped(Decoupled(new WPrfBundle))) 
}


/* 
  IN AND OUT OF FUNCTION UNITS
  we have 4 function Units:
    each RS connect its function Unit (ALURS to 2 )
    ROB TODO:discuss should change to 4 ou ports!!!
    we read prf in "Backend",and connect it to FU
  ALU-MDU-LSU has different PipeLine:
    instantiate stage in funcUnit PipeLine use ↓ these stageIO
  the wb width is 3,now we dicide to block mdu_out if other 3 all produce 
*/


//TODO:  use addsource for load index?
class ReadOpStageIO(useBypass: Boolean = false) extends MycpuBundle {

  val in = Flipped(Decoupled(new Bundle 
  {
    val fromRs   = new InstInfoBundle
    val fromRob  = new RobInfoBundle
    val datasFromPrf  = Vec(srcDataNum,Output(UInt(dataWidth.W)))
    //TODO:if...bypass...
  }))
  val out = Decoupled(new ReadOpStageOutIO)
}

class exeStageIO extends MycpuBundle {
  val in  = Flipped(Decoupled(new ReadOpStageOutIO))
  val out = Decoupled(new exeStageOutIO)
}



class  MemStage1IO extends MycpuBundle{
  val readOpIn = Flipped(Decoupled(new ReadOpStageOutIO))
  val storeQIn = Flipped(Decoupled(new StoreQueueOutIO))
}


