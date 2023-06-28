package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import cache._
import decodemacro.MacroDecode

/**
  * out.predictResult := stage1.out.bpuOut(bpuOut.takenMask become a "taken" bit)
  *
  * out.basicInstInfo := Instructs from I-Cache
  *
  * out.validNum := func(alignMask, bpuOut.takenMask)
  *
  * out.exception := in.exception
  *
  * pass abort signal to cacheStage2 in this stage
  * if tlb is miss or pc is not aligned
  *
  * pay attention
  * 1. cache must keep data until instBuffer has space
  * 2. ready can not be set from in.iCache.cacheInst.valid to cache redirect
  *
  * when miss,use DramReadIO to connect Dram
  * this connect to IfStage1 can use pipeline connect
  * so all io.in is from regs
  */
class IfStage2 extends Module with MycpuParam {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new IfStage1OutIO))
    val out  = Decoupled(new IfStage2OutIO)
    val imem = new DramReadIO

    val noBrMispreRedirect = new FrontRedirctIO
    val bpuUpdate          = Decoupled(new BpuUpdateIO)
  })
  val icache2 = Module(new CacheStage2(IcachRoads, IcachLineBytes)())
  icache2.io.in.valid := io.in.valid
  io.in.ready         := icache2.io.in.ready
  asg(icache2.io.in.bits.fromStage1, io.in.bits.iCache)
  asg(icache2.io.in.bits.isException, io.in.bits.exception =/= FrontExcCode.NONE)
  asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  (0 until fetchNum).foreach(i => {
    io.out.bits.predictResult(i) := io.in.bits.predictResult(i)
    io.out.bits.exception        := io.in.bits.exception
    asg(
      io.out.bits.basicInstInfo(i).pcVal,
      Cat(io.in.bits.pcVal(31, 5), io.in.bits.pcVal(4, 2) + i.U, io.in.bits.pcVal(1, 0))
    )
    asg(io.out.bits.basicInstInfo(i).instr, icache2.io.out.bits.idata.get(i))
    io.out.bits.validMask(i) := io.in.bits.validMask(i)
  })
  io.out.valid         := icache2.io.out.valid
  icache2.io.out.ready := io.out.ready

  /**
    * pre-decode
    *   find out if a no-branch inst have been predicted taken
    *     change its predict result
    *     mask the inst behind it
    *     need to redirect frontend
    *     need to update BPU:
    *         set a queue,when q full,just don't get into Q
    */

  @MacroDecode
  class IF2PreDecodeOut extends MycpuBundle {
    val brType = BranchType()
  }

  import chisel3.util.experimental.decode.QMCMinimizer
  val preDecoder     = Wire(Vec(fetchNum, new IF2PreDecodeOut))
  val nonBrMisPreVec = Wire(Vec(fetchNum, Bool()))
  val firNonBrMispre = WireDefault(0.U(log2Up(retireNum).W))
  val bpuUpdateQueue = Module(new Queue(gen = new BpuUpdateIO, entries = 4))

  //default
  asg(io.noBrMispreRedirect.flush, false.B)
  asg(io.noBrMispreRedirect.target, 0.U(vaddrWidth.W))
  asg(bpuUpdateQueue.io.enq.valid, false.B)
  asg(bpuUpdateQueue.io.enq.bits, 0.U.asTypeOf(new BpuUpdateIO))

  (0 until fetchNum).foreach(i => {
    val instr     = io.out.bits.basicInstInfo(i).instr
    val preRes    = io.out.bits.predictResult(i)
    val take      = preRes.counter > 1.U
    val instValid = io.out.bits.validMask(i)
    preDecoder(i).decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    nonBrMisPreVec(i) := (preDecoder(i).brType === BranchType.non && take && instValid)
  })

  when(nonBrMisPreVec.asUInt.orR) {
    firNonBrMispre := PriorityEncoder(nonBrMisPreVec)
    val misPc     = io.out.bits.basicInstInfo(firNonBrMispre).pcVal
    val misPreRes = io.out.bits.predictResult(firNonBrMispre)
    //mask the inst behind it
    (0 until fetchNum).map(i => { io.out.bits.validMask(i) := (i.U <= firNonBrMispre) })
    //change its preResult
    misPreRes.counter := 0.U
    //redirect frontend
    asg(io.noBrMispreRedirect.flush, true.B)
    asg(io.noBrMispreRedirect.target, misPc + 4.U)
    //enq bpuUpdateQ
    val bpuUpdateEnq = bpuUpdateQueue.io.enq.bits
    asg(bpuUpdateQueue.io.enq.valid, true.B)
    asg(bpuUpdateEnq.pc, misPc)
    asg(bpuUpdateEnq.moreData, 0.U) //TODO:not sure
    //btb
    val updateBtb = bpuUpdateEnq.btb
    asg(updateBtb.valid, true.B)
    asg(updateBtb.bits.target, misPreRes.target) //Dontcare
    asg(updateBtb.bits.instType, BranchType.non)
    //pht
    val updatePht = bpuUpdateEnq.pht
    asg(updatePht.valid, true.B)
    asg(updatePht.bits, 0.U(2.W))
  }

  //bpuUpdateQ deq
  asg(io.bpuUpdate.valid, bpuUpdateQueue.io.deq.valid)
  asg(bpuUpdateQueue.io.deq.ready, io.bpuUpdate.ready)

}
