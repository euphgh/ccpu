package backend

import bundle._
import config._
import backend._
import chisel3._
import chisel3.util._

class Lsu extends FuncUnit(FuType.Lsu) {
  val tlb  = IO(new TLBSearchIO)
  val dram = IO(new DramIO)

  val storeQueue   = Module(new StoreQueue)
  val storeQueueIn = storeQueue.io.in.bits
  val storeInstReq = memStage1.io.out.bits.dCache.dCacheReq.get

  storeQueue.io.in.valid        := memStage1.io.storeQueueEnq
  storeQueueIn.index            := storeInstReq.index
  storeQueueIn.offset           := storeInstReq.offset
  storeQueueIn.memType          := storeInstReq.memType.get
  storeQueueIn.size             := storeInstReq.size
  storeQueueIn.wStrb            := storeInstReq.wStrb
  storeQueueIn.wWord            := storeInstReq.wWord
  storeQueueIn.tagOfMemReqPaddr := memStage1.io.out.bits.tagOfMemReqPaddr

  //memStage1<pp>Mux(roStageOut storeQout)
  val memStage1 = Module(new MemStage1)
  //memStage1<pp>memStage2
  val memStage2 = Module(new MemStage2)
  //in "Bakcend" ,directly use memStage2.io

}
