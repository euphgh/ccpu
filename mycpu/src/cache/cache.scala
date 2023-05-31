package config

import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid
import org.apache.commons.lang3.text.WordUtils

class CacheMeta(hasDirty: Boolean = false) extends MycpuBundle {
  val tag   = UInt(tagWidth.W)
  val dirty = if (hasDirty) Some(Bool()) else None
  val valid = Bool()
}

class CacheStage1OutIO(roads: Int, isDcache: Boolean) extends MycpuBundle {
  val data = Output(Vec(roads, if (isDcache) UWord else Vec(4, UWord)))
  val meta = Output(Vec(roads, new CacheMeta(isDcache)))
  // only Dcache have
  if (isDcache) {
    val size    = Output(UInt(3.W))
    val wWord   = Output(UWord)
    val wStrb   = Output(UInt(4.W))
    val isWrite = Bool()
    val loadSel = Output(LoadSel())
  }
  if (enableCacheInst) {
    val cacheInst = Valid(new Bundle {
      val op    = CacheOp()
      val taglo = UWord
    })
  }
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
    extends MycpuBundle {
  val in = Decoupled(new Bundle {
    val index  = UInt(cacheIndexWidth.W)
    val offset = UInt(cacheOffsetWidth.W)
    if (enableCacheInst) {
      val cacheInst = Valid(new Bundle {
        val op    = CacheOp()
        val taglo = UWord
      })
    }
    if (isDcache) {
      val size    = Output(UInt(3.W))
      val wWord   = Output(UWord)
      val wStrb   = Output(UInt(4.W))
      val memType = Output(MemType())
    }
  })
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
    extends MycpuBundle {
  val in = Decoupled(new Bundle {
    val ptag        = UInt(tagWidth.W)
    val isUncache   = Bool()
    val fromStage1  = new CacheStage1OutIO(roads, isDcache)
    val isException = Bool()
  })
  val out = Decoupled(new Bundle {
    val toUser = userGen
    val data   = Output(if (isDcache) UWord else Vec(fetchNum, UWord))
  })
  if (enableCacheInst) {
    val cacheInst = new Bundle {
      val finish   = Bool()
      val redirect = Bool()
    }
  }
}
