package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

//TODO:here the entry is type output,change to simple signal is easy
class RsEntry(rsEntryKind: Int) extends RsOutIO(kind = rsEntryKind) {
  val valid = Output(Bool())
}

/**
  * allocate → writeIn - ready - select → readOp
  *
  * in.fromRenameStage.ready = rs not full
  *
  * in rename stage,we select a slot
  *
  * writeIn
  *     reuse rsOutIO for port "fromRenameStage"
  *     TODO:because the info written into Rs all need to take to FU?
  *     attention:
  *       io.in.fromRenameStage contain 2 parts:
  *         1.  renameStageOutIO
  *         2.  robIndex is from robOutIO,not renameStageOutIO(but actually is produced in renameStage):
  *               because instantiate rob in renamestage is too troublesome...
  *               and no need to connect robindex into renamestage
  * ready
  *     1.already rdy in renameStage
  *     2.listen to wenPRF...next cycle the rdy bit will ↑
  *     3.wake-up：the selected insts broadCast its destPregAddr
  *     inte：use "wPrf" port
  *         load -> otherRS (MemStage1)<2 bubble>
  *         alu -> otherRS (ReadOp)<1 bubble>
  *     intra：
  *         aluRS -> aluRS (when "selected")<no bubble,need bypass>
  *
  * select
  *     use priority to select ready slot
  *         LSU/MDU/br：not any older insts(in-order)
  *         ALU(not br)：not any rdy&older insts(ooo)
  *     the selected insts broadCast its destPregAddr ,and will leave RS the next cycle  when fire()
  *
  * out
  *     RS keep the info that the insts will use in the FU
  *     decoded：srcAreg used in RO,uOps used in EXE,destAreg for s-rat update
  *     exception：record exception happended in FU,need write to ROB
  *     predictRes：br need it to detect mispredict in EXE
  *
  *     use sEntry(psrc+valid) in RO,use pDest in WB
  *     use robIndex in WB
  */
class RS(rsKind: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromDispatcher =
        if (rsKind == FuType.Alu.id) Vec(aluRsInPorts, Flipped(Decoupled(new RsOutIO(kind = rsKind))))
        else Flipped(Decoupled(new RsOutIO(kind = rsKind)))
      val wPrf = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
    }
    val out =
      if (rsKind == FuType.Alu.id) Vec(aluFuNum, Decoupled(new RsOutIO(kind = rsKind)))
      else Decoupled(new RsOutIO(kind = rsKind))
  })
  //TODO:rsSize
  val rsEntries = RegInit(VecInit(Seq.fill(2)(0.U.asTypeOf(new RsEntry(rsEntryKind = rsKind)))))
}
