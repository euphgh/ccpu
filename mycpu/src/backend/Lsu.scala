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

  val storeQueueSize = 8 //TODO:

  //stage connect
  val memStage1  = Module(new MemStage1)
  val memStage2  = Module(new MemStage2)
  val storeQueue = Module(new Queue(gen = new StoreQueueOutIO, entries = storeQueueSize, hasFlush = true))
  //PipelineConnect(roStage.io.out, memStage1.io.in, memStage1.io.out.fire, io.flush)
  //PipelineConnect(memStage1.io.out, memStage2.io.in, memStage2.io.out.fire, io.flush)
  memStage2.io.out <> io.out

  //deal with mem1 in,mux from storeQ.out and Ro.out
  //TODO:need change here
  val storeQueueToMem1    = new ReadOpStageOutIO(kind = FuType.Lsu)
  val storeQueueDcacheReq = storeQueueToMem1.dCacheReq.get
  val storeQueueOut       = storeQueue.io.deq.bits
  asg(storeQueueDcacheReq.memType.get, storeQueueOut.memType)
  asg(storeQueueDcacheReq.size, storeQueueOut.size)
  asg(storeQueueDcacheReq.wStrb, storeQueueOut.wStrb)
  asg(storeQueueDcacheReq.wWord, storeQueueOut.wWord)
  asg(storeQueueDcacheReq.index, storeQueueOut.index)
  asg(storeQueueDcacheReq.offset, storeQueueOut.offset)

  //connect mem1 to storeQ
  asg(storeQueue.flush, io.flush)
  asg(storeQueue.io.enq.valid, memStage1.io.storeQueueEnq)
  val storeQueueIn  = storeQueue.io.enq.bits
  val mem1DcacheReq = memStage1.io.out.bits.dCache.dCacheReq.get
  asg(storeQueueIn.index, mem1DcacheReq.index)
  asg(storeQueueIn.offset, mem1DcacheReq.offset)
  asg(storeQueueIn.memType, mem1DcacheReq.memType.get)
  asg(storeQueueIn.size, mem1DcacheReq.size)
  asg(storeQueueIn.wStrb, mem1DcacheReq.wStrb)
  asg(storeQueueIn.wWord, mem1DcacheReq.wWord)
  asg(storeQueueIn.tagOfMemReqPaddr, memStage1.io.out.bits.tagOfMemReqPaddr)
}
