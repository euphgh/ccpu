package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg

/**
  * for now,no load inst wake up
  *
  * connect by pipeline
  *   for mem1 in,we set 2 portS for rostage.out and storeQ.out
  *   for mem1 out,we set 1 port for mem2.in(cause storeQ is not in the pipeline)
  *       we only set a storeEnqReq for storeQ.in
  *         connect enq bits in LSU
  *         TODO:if the store exception,don't let it in storeQ?
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
  */

class MemStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromRoStage = Flipped(Decoupled(new ReadOpStageOutIO(kind = FuType.Lsu)))
      val fromStoreQ  = Flipped(Decoupled(new StoreQBasicEntry))
    }
    val out         = Decoupled(new MemStage1OutIO)
    val storeEnqReq = Output(Bool())

    val tlb = new TLBSearchIO
    val dCacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new CacheInstBundle))) else None

    // val out = new Bundle {
    //   val toStoreQ = Decoupled(new StoreQBasicEntry)
    //   val toMem2   = Decoupled(new Mem1ToMem2)
    // }
  })

  val roIn = io.in.fromRoStage.bits
  asg(
    io.storeEnqReq,
    roIn.decoded.specialType === SpecialType.STORE && io.in.fromRoStage.valid && !roIn.exception.happen
  )

  when(io.in.fromRoStage.valid) {
    //TODO:
    //if roIn.decoded.specialType === SpecialType.STORE,no need cacheReq
    //remember not to detect hit or miss in mem2 if is store
  }
  when(io.in.fromStoreQ.valid) {
    //TODO:
    //no need req tlb
  }

}
