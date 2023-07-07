package backend

import bundle._
import config._
import backend._
import chisel3._
import utils._
import chisel3.util._
import chisel3.util.experimental.BoringUtils._

class Lsu extends FuncUnit(FuType.Lsu) {
  val tlb          = IO(new TLBSearchIO)
  val dram         = IO(new DramIO)
  val scommit      = IO(Vec(retireNum, Input(Bool())))
  val robOldestIdx = IO(Input(ROBIdx)) //for uncached load

  // module and alias
  val memStage1   = Module(new MemStage1)
  val memStage2   = Module(new MemStage2)
  val storeQ      = Module(new StoreQueue(8))
  val mem1inRight = memStage1.io.in
  val mem1inLeft  = Wire(Flipped(Decoupled(new MemStage1InIO)))
  val deqSQ       = storeQ.io.deq.req
  val selSQ       = !roStage.io.out.valid || !storeQ.io.remain1
  val roOutBits   = roStage.io.out.bits
  // valid and ready
  mem1inLeft.valid := deqSQ.valid || roStage.io.out.valid
  asg(roStage.io.out.ready, Mux(selSQ, false.B, mem1inLeft.ready))
  asg(deqSQ.ready, Mux(!selSQ, false.B, mem1inLeft.ready))
  // bundle connect
  asg(mem1inLeft.bits.isRoStage, !selSQ)
  asg(mem1inLeft.bits.wbInfo.robIndex, roOutBits.robIndex)
  asg(mem1inLeft.bits.wbInfo.destAregAddr, roOutBits.destAregAddr)
  asg(mem1inLeft.bits.wbInfo.destPregAddr, roOutBits.destPregAddr)
  asg(mem1inLeft.bits.exDetect, roOutBits.exDetect)
  asg(mem1inLeft.bits.memType, roOutBits.uOp.memType.get)
  asg(mem1inLeft.bits.srcData, roOutBits.srcData)
  asg(mem1inLeft.bits.mem1Req.rwReq, Mux(selSQ, deqSQ.bits.rwReq, roOutBits.mem.get.cache.rwReq.get))
  asg(mem1inLeft.bits.mem1Req.ROplus.immOffset, roOutBits.mem.get.immOffset)
  asg(mem1inLeft.bits.mem1Req.ROplus.carryout, roOutBits.mem.get.carryout)
  asg(mem1inLeft.bits.mem1Req.SQplus.pTag, deqSQ.bits.pTag)
  asg(mem1inLeft.bits.mem1Req.SQplus.cAttr, deqSQ.bits.cAttr)
  if (debug) asg(mem1inLeft.bits.debugPC.get, roOutBits.debugPC.get)
  // pipeline connect mem1in and Rostage
  PipelineConnect(mem1inLeft, mem1inRight, memStage1.io.out.toMem2.fire, io.flush)
  // cachein and Rostage, connect not pipeline,
  // do not care cache ready, because it's same with mem1 ready
  memStage1.io.cacheIn.valid := mem1inLeft.fire
  memStage1.io.cacheIn.bits := Mux(
    !selSQ,
    roOutBits.mem.get.cache, {
      val sqCacheReq = Wire(new CacheStage1In(true))
      if (enableCacheInst) {
        sqCacheReq.cacheInst.get.valid := false.B
        sqCacheReq.cacheInst.get.bits  := DontCare
      }
      sqCacheReq.rwReq.get := deqSQ.bits.rwReq
      sqCacheReq
    }
  )

  // pipeline connect storeQ/roStage => mem1Stage
  PipelineConnect(memStage1.io.out.toMem2, memStage2.io.in, memStage2.io.out.fire || memStage2.io.doneSQ, io.flush)

  // mem1 connect with outside
  memStage1.io.robOldestIdx := robOldestIdx
  memStage1.io.tlb <> tlb

  // wire connect mem1Stage => storeQ
  memStage1.io.out.toStoreQ <> storeQ.io.enq
  memStage1.io.stqEmpty := storeQ.io.empty

  // stage2 connect with storeQ
  storeQ.io.deq.back := memStage2.io.doneSQ // when store finish, release storeQ
  memStage2.io.querySQ <> storeQ.io.query // search storeQ while load

  // mem2 to outside
  dram <> memStage2.io.dmem
  memStage2.io.flush := io.flush
  io.out <> memStage2.io.out

  // storeQ to outside
  storeQ.io.flush := io.flush
  (0 until retireNum).foreach(i => { storeQ.io.retire(i) := scommit(i) })
}
