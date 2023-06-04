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
    val in = Flipped(Decoupled(new Bundle {
      val wbRob        = new WbRobBundle
      val destPregAddr = Output(UInt(pRegAddrWidth.W))
      val decoded      = new DecodeInstInfoBundle

      val srcDatas = Vec(2, Output(UInt(dataWidth.W)))

      val cacheIndex  = Output(UInt(cacheIndexWidth.W))
      val cacheOffset = Output(UInt(cacheOffsetWidth.W))

      //this 4 may merge into decode?
      val size    = Output(UInt(3.W))
      val wWord   = Output(UWord)
      val wStrb   = Output(UInt(4.W))
      val memType = Output(MemType())
    }))
    val out = Decoupled(new MemStage1OutIO)
    val tlb = new Bundle {
      val req  = Output(UWord)
      val back = Input(UWord)
    }

    if (enableCacheInst) {
      val IcacheInst = Flipped(Valid(new Bundle {
        val op    = CacheOp()
        val taglo = UWord
        val index = UInt(cacheIndexWidth.W)
        // only need index and tag but offset in cache inst
      }))
    }

  })
}
