package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
class Lsu extends MycpuModule {

  val roStage = new RoStage(fuKind = fuType.Alu.id)
  //for now,no inside bypass

  //TODO:pipeline connect roStage.io<>memStage1
  //TODO:where to instantiate dtlb
  //TODO:storeQueue
  //TODO:dCache
  val memStage1 = new MemStage1
  //TODO:just a memStage1IO? write mem1 logic here?(signal too many-troubleSome)
  //TODO:just a memStage2IO? like mdu and alu?
}
