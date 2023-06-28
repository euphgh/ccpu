package bundle
import chisel3._
import chisel3.util._
import config._

class CacheInstBundle extends MycpuBundle {
  val op    = Output(CacheOp())
  val taglo = Output(UWord)
}

class CacheLowAddr extends MycpuBundle {
  val index  = Output(UInt(cacheIndexWidth.W))
  val offset = Output(UInt(cacheOffsetWidth.W))
}

class CacheRWReq extends MycpuBundle {
  val lowAddr = new CacheLowAddr
  val isWrite = Bool()
  val size    = UInt(3.W)
  val wWord   = UWord
  val wStrb   = UInt(4.W)
}

class CacheMeta(hasDirty: Boolean = false) extends MycpuBundle {
  val tag   = UInt(tagWidth.W)
  val dirty = if (hasDirty) Some(Bool()) else None
  val valid = Bool()
}

class CacheStage1OutIO(roads: Int, wordNum: Int, isDcache: Boolean) extends MycpuBundle {
  val meta = Vec(roads, new CacheMeta(isDcache))
  // ICache
  val idata     = if (!isDcache) Some(Vec(roads, Output(Vec(fetchNum, UWord)))) else None
  val iCacheReq = if (!isDcache) Some(new CacheLowAddr) else None
  // DCache
  val ddata     = if (isDcache) Some(Vec(roads, Output(UWord))) else None
  val dCacheReq = if (isDcache) Some(new CacheRWReq) else None
  val dataline  = if (isDcache) Some(Vec(roads, Output(Vec(wordNum, UWord)))) else None
  // Cache Inst
  val cacheInst = if (enableCacheInst) Some(Valid(new CacheInstBundle)) else None
}

class CacheStage1In(isDcache: Boolean) extends MycpuBundle {
  val rwReq     = if (isDcache) Some(new CacheRWReq) else None
  val ifReq     = if (!isDcache) Some(new CacheLowAddr) else None
  val cacheInst = if (enableCacheInst) Some(Valid(new CacheInstBundle)) else None
}
