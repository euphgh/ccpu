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
  val memStage1 = Module(new MemStage1)
  val memStage2 = Module(new MemStage2)
  val storeQ    = Module(new StoreQueue(8))
  val mem1in    = memStage1.io.in
  val deqSQ     = storeQ.io.deq.req
  val selSQ     = !roStage.io.out.valid || storeQ.io.full
  val roOutBits = roStage.io.out.bits
  // valid and ready
  mem1in.valid := deqSQ.valid || roStage.io.out.valid
  asg(roStage.io.out.ready, Mux(selSQ, false.B, mem1in.ready))
  asg(deqSQ.ready, Mux(!selSQ, false.B, mem1in.ready))
  // bundle connect
  asg(mem1in.bits.isRoStage, !selSQ)
  asg(mem1in.bits.wbInfo.robIndex, roOutBits.robIndex)
  asg(mem1in.bits.wbInfo.destAregAddr, roOutBits.destAregAddr)
  asg(mem1in.bits.wbInfo.destPregAddr, roOutBits.destPregAddr)
  asg(mem1in.bits.exDetect, roOutBits.exDetect)
  asg(mem1in.bits.memType, roOutBits.uOp.memType.get)
  asg(mem1in.bits.srcData, roOutBits.srcData)
  asg(mem1in.bits.mem1Req.rwReq, Mux(selSQ, deqSQ.bits.rwReq, roOutBits.mem.get.cache.rwReq.get))
  asg(mem1in.bits.mem1Req.ROplus.immOffset, roOutBits.mem.get.immOffset)
  asg(mem1in.bits.mem1Req.ROplus.carryout, roOutBits.mem.get.carryout)
  asg(mem1in.bits.mem1Req.SQplus.pTag, deqSQ.bits.pTag)
  asg(mem1in.bits.mem1Req.SQplus.cAttr, deqSQ.bits.cAttr)
  // do not care cache ready, because it's same with mem1 ready
  memStage1.io.cacheIn.valid := mem1in.valid
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
  // when store finish, release storeQ
  storeQ.io.deq.back := memStage2.io.out.valid

  memStage1.io.stqEmpty     := storeQ.io.empty
  memStage1.io.robOldestIdx := robOldestIdx
  memStage1.io.tlb <> tlb
  // pipeline connect storeQ/roStage => mem1Stage
  PipelineConnect(memStage1.io.out.toMem2, memStage2.io.in, memStage2.io.out.fire, io.flush)
  // wire connect mem1Stage => storeQ
  memStage1.io.out.toStoreQ <> storeQ.io.enq

  memStage2.io.out.ready := io.out.ready
  memStage2.io.querySQ <> storeQ.io.query

  dram <> memStage2.io.dmem
  memStage2.io.flush := io.flush
  storeQ.io.flush    := io.flush
  (0 until retireNum).foreach(i => { storeQ.io.retire(i) := scommit(i) })
}
