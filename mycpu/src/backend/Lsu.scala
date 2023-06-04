package backend

import bundle._
import config._
import chisel3._
import chisel3.util._
class Lsu extends MycpuModule {
  //TODO:dtlb port

  //in "Backend" ,directly connect to Lsu.roStage
  val roStage = new RoStage(fuKind = fuType.Alu.id)
  //for now,no inside bypass

  //instantiate storeQ
  //memStage1<pp>Mux(roStageOut storeQout)
  //roStage.io.out.ready:=
  val memStage1 = new MemStage1
  //memStage1<pp>memStage2
  val memStage2 = new MemStage2
  //in "Bakcend" ,directly use memStage2.io

}
