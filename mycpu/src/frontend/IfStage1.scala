package frontend
import bundle._
import config._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid

class BtbOutIO extends MycpuBundle {
  val instType = BranchType()
  val target   = UWord
}

/**
  * single write port and single read port
  * when write and read to same addr in a cycle
  * read data := write data
  *
  * out should keep out until posedge that in.search.valid is true
  */
class BranchTargetBuffer extends MycpuModule {
  val in = new Bundle {
    val search = Valid(UWord)
    val update = Valid(new BtbOutIO)
  }
  val out = Output(new BtbOutIO)
}
class PatternHistoryTable extends MycpuModule {
  val in = new Bundle {
    val search = Valid(UWord)
    val update = Valid(UInt(2.W))
  }
  val out = Output(UInt(2.W))
}

class BpuUpdateIO extends MycpuBundle {
  val pc       = Output(UWord)
  val btb      = Valid(new BtbOutIO)
  val pht      = Valid(UInt(2.W))
  val moreData = Output(UInt(1.W)) //TODO:not make sure
}

/**
  * not connect by pipeline
  * out.pcVal = regEnable(preIFOutIO.npc, fire)
  *
  * out.bpuOut give predict result after fire posedge
  * should keep result until next fire posedge
  *
  * out.alignMask is calculate by out.pcVal
  *
  * out.tagOfInstGroup is calculate by out.pcVal
  * connect TLB module by tlb bundle
  * tlb.req = out.pcVal
  * out.tagOfInstGroup  = tlb.back
  *
  * out.exception != NONE when tlb exception or address error
  *
  * instantiate I-cache stage1 in this module
  * all out should not change until next fire posedge
  * when in.flush || IcacheInst.valid = true
  * stage1 must update all out in next cycle
  *
  * instantiate BPU in this module, let out.bpuout = bpu.out
  */
class IfStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(new PreIfOutIO)
    val out = Decoupled(new IfStage1OutIO)
    val tlb = new Bundle {
      val req  = Output(UWord)
      val back = Input(UWord)
    }
    val bpuUpdateIn = Flipped(new BpuUpdateIO)
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
