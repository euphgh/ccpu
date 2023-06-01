package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * robEntry:
  *  valid
  *
  *  pc?(basic)
  *  instr?(basic)
  *  prevDestPregAddr
  *
  *  memReqVaddr(wbIn)---fu produce
  *  exception(wbIn)---fu produce
  *  isMispredict-Boll(wbIn)---fu produce
  */
class RobEntry extends MycpuBundle {
  val valid = Bool()

  val basic            = new BasicInstInfoBundle
  val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))

  val exception    = new ExceptionInfoBundle
  val isMispredict = Output(Bool())
  val memReqVaddr  = Output(UInt(vaddrWidth.W))
}

/**
  * robIndex is actually the headPtr
  *   change logic:fire()
  *   connect to RS in Backend Module,use one port:
  *     inst(0).index:=rob.out.robIndex  inst(1).index:=rob.out.index+1
  *     not need to decoupled index,cause this logic is dealt in io.in.ready
  *
  * after rename stage
  *   rob use "robindex" to write port <fromRenameStage> into the slot
  *     basic:TODO:may delete instr?
  *       PC/instr
  *     prevDestPregAddr
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
  *       change the a-rat
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
      val fromRenameStage = Vec(
        dispatchNum,
        Flipped(Decoupled(new Bundle {
          val prevDestPregAddr = Output(UInt(pRegAddrWidth.W))
          val basic            = new BasicInstInfoBundle
        }))
      )
      val wbRob = Vec(wBNum, Flipped(Decoupled(new WbRobBundle)))
    }
    val out = new Bundle {
      val robIndex    = Output(UInt(robIndexWidth.W))
      val toRetire    = Vec(retireNum, Decoupled(new RetireBundle))
      val storeCommit = Output(Bool())
    }
  })
}
