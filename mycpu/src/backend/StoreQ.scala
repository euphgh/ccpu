package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg

//this bundle is used when store inst get into mem1
//cache basic req contain index/offset
class StoreQBasicEntry extends CacheBasicReq {
  val tagOfMemReqPaddr = Output(UInt(tagWidth.W)) //get in mem1

  val size    = Output(UInt(3.W)) //gen in RO
  val wWord   = Output(UWord) //read in RO
  val wStrb   = Output(UInt(4.W)) //
  val memType = Output(MemType()) //FIXME:
}

//retired:retire from rob
//done:really write into mem
class StoreQueueEntry extends MycpuBundle {
  val basic   = new StoreQBasicEntry
  val retired = Output(Bool())
  val done    = Output(Bool())
}

class StoreQueue extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new StoreQBasicEntry))
    val out = Decoupled(new StoreQBasicEntry)

    val full  = Output(Bool())
    val flush = Input(Bool())

    val storeRetire  = Input(Bool()) //TODO:set to retire num?
    val storeReqDone = Input(Bool())
  })

  //TODO:storeQ size
  val storeQueueSize = 8
  val storeQEntries  = Module(new Queue(gen = new StoreQueueEntry, entries = storeQueueSize, hasFlush = true))
  asg(storeQEntries.flush, io.flush)
  asg(io.full, storeQEntries.full)

  //storeQ enq ~ io.in
  val storeEnqBits = storeQEntries.io.enq.bits
  asg(storeQEntries.io.enq.valid, io.in.valid)
  asg(storeEnqBits.basic, io.in.bits)
  asg(storeEnqBits.done, false.B)
  asg(storeEnqBits.done, false.B)
  asg(io.in.ready, storeQEntries.io.enq.ready)

  //storeQ deq
  //attention:出队和io.out无关
  val storeDeqBits = storeQEntries.io.deq.bits
  asg(storeQEntries.io.deq.ready, storeDeqBits.done)

  //io.out
  //retired deqInst can call a memReq
  asg(io.out.valid, storeQEntries.io.deq.valid && storeDeqBits.retired)
  asg(io.out.bits, storeDeqBits.basic)

  /**
    *   when store inst retire from rob, set its retired bit
    *     TODO:should we use a storeQIdx?because 2nd inst can be set retired
    *   when store inst done req,set its reqDone bit
    *     only head store inst will be set done!
    */
  asg(storeQEntries.ram(storeQEntries.deq_ptr.value).done, io.storeReqDone)
}
