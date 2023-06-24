package backend
import config._
import backend._
import bundle._
import chisel3._
import utils._

class Alu(main: Boolean) extends FuncUnit(FuType.MainAlu) {
  val extInt = if (main) Some(IO(UInt(6.W))) else None

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
  asg(exeOut.wbRob.takeWord, exeIn.srcData(1)) //only mtc0 care
  asg(exeOut.wbRob.isMispredict, false.B) //default,mainAlu may change it
  asg(exeOut.wbRob.robIndex, exeIn.robIndex)

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
}
