package backend

import bundle._
import config._
import chisel3._
import chisel3.util._

class Mdu extends MycpuModule {

  val roStage = Module(new RoStage(fuKind = FuType.Mdu.id))
  //for now,no inside bypass

  //TODO:pipeline connect roStage.io<>exeStageIO
  val exeStageIO = new ExeStageIO(fuKind = FuType.Alu.id)
  /*
  TODO:
    exe logic(real MDU)
   */

}
