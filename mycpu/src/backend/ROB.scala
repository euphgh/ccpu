package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

class RobEntry extends MycpuBundle {
  val valid = Bool()

  val pc               = UWord
  val destAregAddr     = Output(UInt(aRegAddrWidth.W))
  val destPregAddr     = Output(UInt(pRegAddrWidth.W))
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
  //val fromDispatcher = new DispatchInRobBundle

  val exception    = new ExceptionInfoBundle
  val isMispredict = Output(Bool())
  val memReqVaddr  = Output(UInt(vaddrWidth.W))
}

/**
  * robIndex is actually the headPtr
  *   change logic:fire()
  *   connect to Dispatcher in Backend Module,use 3 ports
  *     need to decoupled,Dispatcher use it to gen "Dispatch.valid"
  *
  * after rename stage
  *   rob use "robindex" to write port <fromRenameStage> into the slot
  *     pc---for difftest
  *     prevDestPregAddr---for freelist
  *   rs write "robindex" into its slot
  *
  * after exeStage(Mem2Stage),write port <wbRob> into ROB:
  *   exception
  *   isMispredict
  *   memReqVaddr
  *
  * the oldest insts should retire from rob
  *     1.normally
  *       just use <prevDestPregAddr> to push freelist
  *       change the a-rat:
  *            use destPregAddr/destAregAddr
  *       TODO:give cp0 wen!!!
  *     2.special(exception|mispredict)
  *       exception
  *         <exception>:to CP0
  *         <nextTarget>
  *           use nextTarget and exception.happen as <FrontRedirctIO> to preIf
  *       mispredict
  *         flushBackend(will unlock renameStage.valid)
  *     3.storeInst
  *       send a scommit to storeQueue  TODO:need to add bit to robEntry
  */

class ROB extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromDispatcher = Vec(
        dispatchNum,
        Flipped(Decoupled(new DispatchToRobBundle))
      )
      val wbRob = Vec(wBNum, Flipped(Decoupled(new WbRobBundle)))
    }
    val out = new Bundle {
      val robIndex    = Vec(renameNum, Decoupled(Output(ROBIdx)))
      val toRetire    = Vec(retireNum, Decoupled(new RetireBundle))
      val storeCommit = Decoupled(Output(Bool()))
    }
  })
}
