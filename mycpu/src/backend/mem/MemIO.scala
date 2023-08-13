package backend.mem
import chisel3._
import chisel3.util._
import cache._
import config._
import bundle._
import config.MycpuObject.DcachLineBytes

//this bundle is used when store inst get into mem1
//cache basic req contain index/offset
class StoreQIO extends MycpuBundle {
  val pTag    = UInt(tagWidth.W) //get in mem1
  val cAttr   = CCAttr()
  val rwReq   = new CacheRWReq(DcachLineBytes)
  val debugPC = if (debug) Some(Output(UWord)) else None
}

class WriteBackIO extends MycpuBundle {
  val robIndex     = UInt(robIndexWidth.W)
  val destPregAddr = UInt(pRegAddrWidth.W)
  val destAregAddr = ARegIdx
}

class SQplusReq extends MycpuBundle {
  val pTag  = UInt(tagWidth.W)
  val cAttr = CCAttr()
}

class Mem1ReqBus extends MycpuBundle {
  val SQplus = new SQplusReq
}

class MemStage1InIO extends MycpuBundle {
  val wbInfo    = new WriteBackIO
  val exDetect  = new DetectExInfoBundle
  val memType   = MemType()
  val srcData   = Vec(2, Output(UInt(dataWidth.W)))
  val preDstSrc = UWord
  val rwReq     = new CacheRWReq(DcachLineBytes)
  val cacheInst = if (enableCacheInst) Some(Flipped(Valid(new CacheInstBundle))) else None
  val carryOut  = Output(Bool())
  val immOffset = Output(UInt(16.W))
  val rLowAddr  = Output(new CacheLowAddr(DcachLineBytes))
  val mipOut    = Flipped(Valid(new IndexPredictor.MIPOutIO))
  val idxMiss   = Output(Bool())
  val dirCattr  = CCAttr()
  val isDir     = Bool()
  val pcVal     = UWord
  val debugPC   = if (debug) Some(UWord) else None
}

class MemStage1OutIO extends MycpuBundle {
  val isSQ       = Bool()
  val isWuart    = Bool()
  val wbInfo     = new WriteBackIO
  val prevDstSrc = UWord
  val exDetect   = new DetectExInfoBundle
  val toCache2   = new CacheStage1OutIO(DcachRoads, DcachLineBytes / 4, true)
  val pTag       = UInt(tagWidth.W)
  val isUncache  = Bool()
  val memType    = MemType()
  val debugPC    = if (debug) Some(UWord) else None
}

// needMask = sqMask | memMask
// if memMask === 0.U, all need data in sq
// should cancel Dcache
class QuerySQOut extends MycpuBundle {
  val data    = UWord
  val sqMask  = UInt(4.W)
  val memMask = UInt(4.W)
}
class QuerySQIn extends MycpuBundle {
  val addr     = UWord // do not care about last 2 bits
  val needMask = UInt(4.W)
}

class QuerySQ extends MycpuBundle {
  val req = Output(new QuerySQIn)
  val res = Input(new QuerySQOut)
}

class Mem1ToStqIO extends MycpuBundle {
  val stqEnq   = new StoreQIO //mem1 has ro store
  val wbInfo   = new WriteBackIO
  val exDetect = new DetectExInfoBundle
  val scFail   = Bool()
  val isSC     = Bool()
}
