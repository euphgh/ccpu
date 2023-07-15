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
  val scommitPC    = if (debug) Some(IO(Vec(retireNum, Input(UWord)))) else None
  val stqEmpty     = IO(Bool())
  val oldestRobIdx = IO(Input(ROBIdx))

  // module and alias
  val memStage1 = Module(new MemStage1)
  val memStage2 = Module(new MemStage2)
  val storeQ    = Module(new StoreQueue(8))
  val roOutBits = roStage.io.out.bits
  val deqSQ     = storeQ.io.deq.req
  val mem1RO    = memStage1.io.fromRO
  val mem1SQ    = memStage1.io.fromSQ

  roStage.io.out.ready := memStage1.io.fromRO.ready
  mem1RO.valid         := roStage.io.out.valid
  mem1SQ.valid         := deqSQ.valid
  deqSQ.ready          := mem1SQ.ready
  // bundle connect
  asg(mem1RO.bits.wbInfo.robIndex, roOutBits.robIndex)
  asg(mem1RO.bits.wbInfo.destAregAddr, roOutBits.destAregAddr)
  asg(mem1RO.bits.wbInfo.destPregAddr, roOutBits.destPregAddr)
  asg(mem1RO.bits.exDetect, roOutBits.exDetect)
  asg(mem1RO.bits.memType, roOutBits.uOp.memType.get)
  asg(mem1RO.bits.srcData, roOutBits.srcData)
  asg(mem1RO.bits.preDstSrc, roOutBits.prevData)
  asg(mem1RO.bits.rwReq, roOutBits.mem.get.cache.rwReq.get)
  asg(mem1RO.bits.immOffset, roOutBits.mem.get.immOffset)
  asg(mem1RO.bits.carryout, roOutBits.mem.get.carryout)
  asg(mem1RO.bits.cacheInst.get, roOutBits.mem.get.cache.cacheInst.get)
  asg(mem1RO.bits.debugPC.get, roOutBits.debugPC.get)
  asg(mem1SQ.bits.pTag, deqSQ.bits.pTag)
  asg(mem1SQ.bits.cAttr, deqSQ.bits.cAttr)
  asg(mem1SQ.bits.rwReq, deqSQ.bits.rwReq)
  asg(mem1SQ.bits.debugPC.get, deqSQ.bits.debugPC.get)
  asg(memStage1.io.oldestRobIdx, oldestRobIdx)

  // pipeline connect storeQ/roStage => mem1Stage
  memStage1.io.out.toStoreQ <> storeQ.io.fromMem1
  storeQ.io.ldFire := memStage1.io.out.toMem2.fire && !memStage1.io.out.toMem2.bits.isSQ
  //PipelineConnect(memStage1.io.out.toStoreQ, storeQ.io.fromMem1, storeQ.io.writeBack.fire, io.flush)

  // pipeline connect storeQ/roStage => mem1Stage
  val mem2Flush =
    Mux(
      (memStage2.io.in.bits.isSQ && memStage2.io.in.valid && !memStage2.io.doneSQ) || (memStage1.io.out.toMem2.bits.isSQ && memStage1.io.out.toMem2.fire),
      false.B,
      io.flush
    )
  PipelineConnect(memStage1.io.out.toMem2, memStage2.io.in, memStage2.io.out.fire || memStage2.io.doneSQ, mem2Flush)

  memStage1.io.tlb <> tlb
  memStage1.io.flush := io.flush

  // wire connect mem1Stage => storeQ
  // memStage1.io.out.toStoreQ <> storeQ.io.enq
  memStage1.io.stqEmpty := storeQ.io.empty

  // stage2 connect with storeQ
  storeQ.io.deq.back := memStage2.io.doneSQ // when store finish, release storeQ
  memStage2.io.querySQ <> storeQ.io.query // search storeQ while load
  storeQ.io.deq.backPC.get := memStage2.io.donePC.get // when store finish, release storeQ

  // mem2 to outside
  dram <> memStage2.io.dmem
  memStage2.io.flush := io.flush
  io.out <> memStage2.io.out

  // storeQ to outside
  storeQ.io.flush := io.flush
  (0 until retireNum).foreach(i => {
    storeQ.io.retire(i)                  := scommit(i)
    if (debug) storeQ.io.retirePC.get(i) := scommitPC.get(i)
  })
  storeQ.io.writeBack.ready := io.out.ready
  stqEmpty                  := storeQ.io.empty

  io.out.valid := storeQ.io.writeBack.valid || memStage2.io.out.valid
  // select writeback
  memStage2.io.out.ready    := io.out.ready
  storeQ.io.writeBack.ready := !memStage2.io.out.valid && io.out.ready
  asg(io.out.bits, Mux(memStage2.io.out.valid, memStage2.io.out.bits, storeQ.io.writeBack.bits))
}
