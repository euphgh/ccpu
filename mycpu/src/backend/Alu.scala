package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
class Alu extends MycpuModule {

  val roStage = new RoStage(fuKind = fuType.Alu.id)
  //inside bypass
  roStage.io.datasFromBypass.get(1) := exeStageIO.out.bits.wPrf

  //TODO:pipeline connect roStage.io<>exeStageIO

  val exeStageIO = new ExeStageIO(fuKind = fuType.Alu.id)
  /*
  TODO:
    exe logic(real ALU)
   */

}
