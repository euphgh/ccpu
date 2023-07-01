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
    val in      = Flipped(Decoupled(new IfStage1OutIO))
    val out     = Decoupled(new IfStage2OutIO)
    val imem    = new DramReadIO
    val flushIn = Input(Bool())

    val noBrMispreRedirect = new FrontRedirctIO
    val bpuUpdate          = Decoupled(new BpuUpdateIO)
  })
  val icache2 = Module(new CacheStage2(IcachRoads, IcachLineBytes)())
  icache2.io.in.valid := io.in.valid
  io.in.ready         := icache2.io.in.ready
  asg(icache2.io.in.bits.fromStage1, io.in.bits.iCache)
  asg(icache2.io.in.bits.cancel, io.in.bits.exception =/= FrontExcCode.NONE)
  asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  val inBits  = io.in.bits
  val outBits = io.out.bits
  (0 until fetchNum).foreach(i => {
    val outBasic = outBits.basicInstInfo(i)
    val inPcVal  = inBits.pcVal
    outBits.predictResult(i) := inBits.predictResult(i)
    asg(outBasic.pcVal, Cat(inPcVal(31, 5), inPcVal(4, 2) + i.U, inPcVal(1, 0)))
    asg(outBasic.instr, icache2.io.out.bits.idata.get(i)) //要求cache根据自动机状态返回全0
    //asg(outBasic.instr, Mux(inBits.exception === FrontExcCode.AdEL, 0.U(32.W), icache2.io.out.bits.idata.get(i)))
    outBits.validMask(i) := inBits.validMask(i)
  })
  asg(outBits.exception, inBits.exception)
  asg(io.out.valid, icache2.io.out.valid)
  asg(icache2.io.out.ready, io.out.ready)

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
    val brType = BranchType.NON
  }
  import chisel3.util.experimental.decode.QMCMinimizer
  val preDecoder     = Wire(Vec(fetchNum, new IF2PreDecodeOut))
  val validBr        = Wire(Vec(fetchNum, Bool()))
  val nonBrMisPreVec = Wire(Vec(fetchNum, Bool()))
  val firNonBrMispre = WireDefault(0.U(log2Up(retireNum).W))
  val bpuUpdateQueue = Module(new Queue(gen = new BpuUpdateIO, entries = 4))

  //default
  asg(io.noBrMispreRedirect.flush, false.B)
  asg(io.noBrMispreRedirect.target, 0.U(vaddrWidth.W))
  asg(bpuUpdateQueue.io.enq.valid, false.B)
  asg(bpuUpdateQueue.io.enq.bits, 0.U.asTypeOf(new BpuUpdateIO))
  //asg(bpuUpdateQueue.io.flush.get, io.flush)

  //predecode
  (0 until fetchNum).foreach(i => {
    val instr     = io.out.bits.basicInstInfo(i).instr
    val preRes    = io.out.bits.predictResult(i)
    val preTake   = preRes.counter > 1.U
    val instValid = io.out.bits.validMask(i) && io.in.valid
    preDecoder(i).decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    val realBrType = preDecoder(i).brType
    nonBrMisPreVec(i) := (realBrType === BranchType.NON && preTake && instValid) //注意io.in.valid
    asg(io.out.bits.realBrType(i), realBrType)
    asg(validBr(i), realBrType =/= BranchType.NON && instValid) //注意io.in.valid
  })

  when(nonBrMisPreVec.asUInt.orR && io.in.valid) {
    firNonBrMispre := PriorityEncoder(nonBrMisPreVec)
    val misPc     = io.out.bits.basicInstInfo(firNonBrMispre).pcVal
    val misPreRes = io.out.bits.predictResult(firNonBrMispre)
    //mask the inst behind it
    (0 until fetchNum).map(i => { io.out.bits.validMask(i) := (i.U <= firNonBrMispre) })
    //change its preResult
    misPreRes.counter := 0.U
    misPreRes.btbType := BtbType.non
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
    asg(updateBtb.bits.instType, BtbType.non)
    //pht
    val updatePht = bpuUpdateEnq.pht
    asg(updatePht.valid, true.B)
    asg(updatePht.bits, 0.U(2.W))
  }
  //bpuUpdateQ deq
  asg(io.bpuUpdate.valid, bpuUpdateQueue.io.deq.valid)
  asg(bpuUpdateQueue.io.deq.ready, io.bpuUpdate.ready)

  /**
    * is延迟槽
    *   目前放在IF2段处理
    *   PRE-IF那还不太一样，如果分支没被预测跳转，是不会进入单走延迟槽的状态的
    */
  val validMask = io.in.bits.validMask
  val dsReg     = RegInit(false.B)
  val lastDs    = WireInit(0.U((1 + log2Up(fetchNum)).W))
  val validNum  = PriorityCount(validMask.asUInt)
  when(validMask(0) && io.in.valid) { dsReg := false.B } //注意io.in.valid
  when(validBr.asUInt.orR && io.in.valid) {
    asg(lastDs, fetchNum.U - PriorityEncoder(validBr.reverse)) //最后一个延迟槽对应取过来的四条指令的哪一个
    when(lastDs >= validNum) {
      dsReg := true.B
    }
  }
  val isBd = io.out.bits.isBd
  (0 until (fetchNum - 1)).map(i => asg(isBd(i + 1), validBr(i)))
  asg(isBd(0), dsReg)
  when(io.flushIn) { dsReg := false.B }
}
