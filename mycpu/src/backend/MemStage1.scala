package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * TODO: load inst wake up :use addsink?
  *
  * connect by pipeline
  * in = Mux (RoStage.out and StoreQ.out)
  *   use port<Flipped RoStageOut>as MemStage1.in
  *   if the in is storeQ.out,should fill some DontCare signal
  *
  * use in.decoded.offset and in.srcData1 to cal 32bit vaddr
  * tlb.req:=vaddr
  * take tlb.back.tag to next stage
  *
  * out.exception != NONE when tlb exception or address error
  *   out.wbRob will change(memReqVaddr exception)
  *
  * instantiate D-cache stage1 in this module
  *   in.cacheIdx/in.cacheoffset/that 4 directly connect to dCache1.in
  *   connect dcache_1.out to MemStage1OutIO.dcache
  *   the "enableCacheInst" is for cacheInst
  *
  * TODO: storeEnq signal
  *     rise storeQEnq signal if the inst is store
  */

class MemStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in            = Flipped(Decoupled(new ReadOpStageOutIO(kind = FuType.Lsu)))
    val out           = Decoupled(new MemStage1OutIO)
    val tlb           = new TLBSearchIO
    val storeQueueEnq = Output(Bool())
    val dCacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new CacheInstBundle))) else None
  })
}
