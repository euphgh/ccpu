package backend

import bundle._
import config._
import backend._
import chisel3._
import utils._
import chisel3.util._

//TODO:must set isMispredict = false
class Lsu extends FuncUnit(FuType.Lsu) {
  val tlb  = IO(new TLBSearchIO)
  val dram = IO(new DramIO)
  //TODO:scommit IO

  //stage connect
  //note that mem1 in has 2 port,out is only 1 port(connect to mem2)
  //in.valid already mean instvalid
  val memStage1 = Module(new MemStage1)
  val memStage2 = Module(new MemStage2)
  val storeQ    = Module(new StoreQueue)
  PipelineConnect(roStage.io.out, memStage1.io.in.fromRoStage, memStage1.io.out.fire, io.flush)
  PipelineConnect(storeQ.io.out, memStage1.io.in.fromStoreQ, memStage1.io.out.fire, io.flush)
  PipelineConnect(memStage1.io.out, memStage2.io.in, memStage2.io.out.fire, io.flush)
  memStage2.io.out <> io.out

  //deal with out valid
  //store can't go to storeQ,thus it is blocked in mem1
  //notice store inst from io.in.fromStoreQ no need go to storeQ
  val mem1RdyGo = !(memStage1.io.storeEnqReq && !storeQ.io.in.ready)
  asg(memStage1.io.out.valid, (memStage1.io.in.fromRoStage.valid || memStage1.io.in.fromStoreQ.valid) && mem1RdyGo)
  asg(storeQ.io.in.valid, memStage1.io.storeEnqReq && !memStage2.io.in.ready)

  //deal with in.ready
  //deal with mem req compete
  asg(memStage1.io.in.fromStoreQ.ready, !(roStage.io.out.valid && !storeQ.io.full))
  asg(memStage1.io.in.fromRoStage.ready, !storeQ.io.full)

  //connect mem1 to storeQ
  val storeQueueIn  = storeQ.io.in.bits
  val mem1DcacheReq = memStage1.io.out.bits.dCache.dCacheReq.get
  asg(storeQueueIn.index, mem1DcacheReq.index)
  asg(storeQueueIn.offset, mem1DcacheReq.offset)
  asg(storeQueueIn.memType, mem1DcacheReq.memType.get)
  asg(storeQueueIn.size, mem1DcacheReq.size)
  asg(storeQueueIn.wStrb, mem1DcacheReq.wStrb)
  asg(storeQueueIn.wWord, mem1DcacheReq.wWord)
  asg(storeQueueIn.tagOfMemReqPaddr, memStage1.io.out.bits.tagOfMemReqPaddr)

  //other bits for storeQ
  asg(storeQ.io.flush, io.flush)
  //TODO:scommit set to retire num?
  //TODO:storeReqDone
}
