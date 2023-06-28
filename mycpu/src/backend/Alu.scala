package backend
import config._
import backend._
import bundle._
import chisel3._
import utils._
import chisel3.util.experimental.BoringUtils

class Alu(main: Boolean) extends FuncUnit(FuType.MainAlu) {

  //stage connect
  val exeStageIO = new ExeStageIO(FuType.MainAlu)
  exeStageIO.out <> io.out
  PipelineConnect(roStage.io.out, exeStageIO.in, exeStageIO.out.fire, io.flush)
  val exeIn  = exeStageIO.in.bits
  val exeOut = exeStageIO.out.bits

  //注意，这里的in.valid已经代表着pipex_valid
  val exeStageReadyGo = true.B
  exeStageIO.out.valid := exeStageIO.in.valid && exeStageReadyGo
  exeStageIO.in.ready  := !exeStageIO.in.valid || exeStageIO.out.ready && exeStageReadyGo

  //unchange signal
  asg(exeOut.destAregAddr, exeIn.destAregAddr)
  asg(exeOut.wPrf.pDest, exeIn.destPregAddr)
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)

  //may change signal
  val outExInfo = exeOut.wbRob.exception
  asg(outExInfo, exeIn.exception) //when exception occur,may change it
  asg(exeOut.wbRob.isMispredict, false.B) //mainAlu may change it

  /**
    * real alu logic:mainAlu/SubAlu
    *
    * gen:
    *   1.wprf.result
    *   2.wRob.exception
    *   3.wRob.isMispredict(for mainAlu)
    */
  val srcs = exeIn.srcData
  //...

  /**
    * deal with interrupt
    *   int has highest priority
    *   attach it to a non-BR inst to prevent(mispre & exception)when retire
    *   实战p172
    */
  if (!main) {
    val hasInt = Wire(Bool())
    BoringUtils.addSink(hasInt, "hasInterrupt")
    when(hasInt) {
      asg(outExInfo.excCode, ExcCode.Int)
      asg(outExInfo.happen, true.B)
      asg(outExInfo.refill, false.B)
    }
  }
}
