package backend

import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * pass abort signal to cacheStage2 in this stage:in.wbRob.exception
  *   if exception happen,dont do anything
  *
  * pay attention
  * 1. Mem2 won't be block by WB conflict(block mdu)
  * 2. ready can not be set from in.dCache.cacheInst.valid to cache redirect
  *
  * instantiate cacheStage2 here,give it
  *    ptag
  *    isUncache
  *    fromStage1
  *    isException
  * when miss,use DramReadIO to connect Dram
  */
class MemStage2 extends MycpuModule {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new MemStage1OutIO))
    val out  = Decoupled(new FunctionUnitOutIO)
    val dmem = new DramIO
  })
}
