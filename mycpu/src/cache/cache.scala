package config //why?

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid
import org.apache.commons.lang3.text.WordUtils

//for cache stage1 in and out
class CacheInstBundle extends MycpuBundle {
  val op    = Output(CacheOp())
  val taglo = Output(UWord)
}
class CacheBasicReq extends MycpuBundle {
  val index  = Output(UInt(cacheIndexWidth.W))
  val offset = Output(UInt(cacheOffsetWidth.W))
}
//for dCache interface IO
class DcacheReq(toCacheStage: Int) extends CacheBasicReq {
  val size  = Output(UInt(3.W))
  val wWord = Output(UWord)
  val wStrb = Output(UInt(4.W))

  val memType = if (toCacheStage == 1) Some(Output(MemType())) else None
  val isWrite = if (toCacheStage == 2) Some(Output(Bool())) else None
  val loadSel = if (toCacheStage == 2) Some(Output(LoadSel())) else None
}

class CacheMeta(hasDirty: Boolean = false) extends MycpuBundle {
  val tag   = Output(UInt(tagWidth.W))
  val dirty = if (hasDirty) Some(Output(Bool())) else None
  val valid = Output(Bool())
}

class CacheStage1OutIO(roads: Int, isDcache: Boolean) extends MycpuBundle {
  val data = Vec(roads, Output(if (isDcache) UWord else Vec(4, UWord)))
  val meta = Vec(roads, new CacheMeta(isDcache))

  //TODO:not for sure:
  //index和offset应该也要带到CacheStage2?
  //这里的写法可能也得改一改
  val dCacheReq = if (isDcache) Some(new DcacheReq(toCacheStage = 2)) else None
  val iCacheReq = if (!isDcache) Some(new CacheBasicReq) else None
  val cacheInst = if (enableCacheInst) Some(Valid(new CacheInstBundle)) else None

  /**
    * TODO: 这样写找不到成员
    * if (isDcache) {
    *    val forDache = new DcacheExtraReq(toCacheStage = 2)
    *  }
    *  if (enableCacheInst) {
    *    val cacheInst = Valid(new CacheInstBundle)
    *  }
    */

}

/**
  * I-cache stage1 not block, it always give data in the cycle next to in.valid cycle
  * if I-cache stage2 not ready, stage1 will lose old data by writing new data
  * so it in.ready can always be true
  * but D-cache stage1 may block for stage 2 not ready
  *
  * in. {size, wWord, sStrb} is from store queue, will be pass to next stage
  *
  * in.memType is from Decode, will be calculate, and turn to out. {isWrite, loadSel}
  *
  * stage1 has 4 8to1 mux, select word from SRAM line by offset
  *
  * cacheInst.op.valid meaning this is a Cache Inst
  * D-stage1 only need pass op and read meta for D-stage2
  * D-stage1 is always not block for no inst in pipeline
  * tlb should read from mem unit to pass D-cache stage2
  * if exception happen, D-cache stage2 should not do any thing
  * set OK D-cache Inst signal in next cycle
  *
  * I-cache stage1 should accept it like flush, no matter it has data or not
  * it can be pass by by `addSink` and `addSource` from mem unit
  * same time, D-stage1 accept a "fake" cacheInstOp meaning reflect I-cache
  * this design is for I-cacheInst can flow like D-cacheInst in pipeline
  * tlb should be read from D-tlb and I-tlb,
  * when I-tlb pass it to I-stage2, D-tlb pass exception to D-stage2
  * if tlb has exception I-stage2 perform like D-stage2, doing nothing
  * set OK I-cache Inst signal in next cycle
  * when D-stage2 accept the "fake" cacheInstOp, it listens and reflect
  * I-stage2's finish signal by `addSink` and `addSource` function.
  */
class CacheStage1(
  roads:     Int     = 4,
  lineBytes: Int     = 8,
  isDcache:  Boolean = false)
    extends MycpuModule {
  val in = Flipped(Decoupled(new Bundle {
    val req       = if (isDcache) new DcacheReq(toCacheStage = 1) else new CacheBasicReq
    val cacheInst = if (enableCacheInst) Some(Valid(new CacheInstBundle)) else None
  }))
  val out = new CacheStage1OutIO(roads, isDcache)
}

/**
  * cache stage2 may block, need Decoupled input
  * output is Decoupled becasue instBuffer
  *
  * stage2 will calculate hit or miss
  * tag, isUncache, isException can only be known after TLB
  * if isUncache, cache must access DRAM
  * if isException, cache must return valid in next cycle and not change Cache status
  *
  * userGen is only used to pre-Decode in I-Cache
  * if no pre-Decode or is Dcache, can pass userGen UInt(0.W)
  * stage2 call trans func to pre-Decode after roads selection
  *
  * pay attention !!!
  * out.data in I-cache is four word(inst)
  * out.data is not load word in Dcache, but "selected load word" by loadSel
  * it can by direct write into prf
  *
  * in.fromStage1.cacheOp.valid meaning that it cache Inst
  * stage2 should only take corresponding action by input
  * note that if isException must do nothing and set OK signal
  * when stage2 set cacheInst OK signal,
  * it not set ready signal for preventing cache pollution
  * it waiting redirect signal to set it ready
  */
class CacheStage2[T <: Data](
  roads:     Int     = 4,
  lineBytes: Int     = 8,
  isDcache:  Boolean = false,
  userGen:   T
)(trans:     (UInt => T))
    extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val ptag        = Output(UInt(tagWidth.W))
      val isUncached  = Output(Bool())
      val fromStage1  = new CacheStage1OutIO(roads, isDcache)
      val isException = Output(Bool())
    }))

    val out = Decoupled(new Bundle {
      val toUser = Output(userGen)
      val data   = Output(if (isDcache) UWord else Vec(fetchNum, UWord))
    })

    val cacheInst = new Bundle {
      val finish   = if (enableCacheInst) Some(Output(Bool())) else None
      val redirect = if (enableCacheInst) Some(Input(Bool())) else None
    }
  })
}
