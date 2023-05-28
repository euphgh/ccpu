package frontend
import bundle._
import config._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid

class BtbOutIO extends MycpuBundle {
  val instType = BpuType()
  val target   = UInt(vaddrWidth.W)
}

class BpuUpdateIO extends MycpuBundle {
  val pc       = Output(Word)
  val btb      = Valid(new BtbOutIO)
  val pht      = Valid(UInt(2.W))
  val moreData = Output(UInt(1.W)) //TODO:not make sure
}

/**
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
  * instantiate I-cache stage1 in this module
  * all out should not change until next fire posedge
  * when in.flush = true, stage1 must update all out in next cycle
  *
  * instantiate BPU in this module, let out.bpuout = bpu.out
  */
class IfStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(new PreIfOutIO)
    val out = Decoupled(new IfStage1OutIO)
    val tlb = new Bundle {
      val req  = Output(Word)
      val back = Input(Word)
    }
    val bpuUpdateIn = Flipped(new BpuUpdateIO)
  })
}
