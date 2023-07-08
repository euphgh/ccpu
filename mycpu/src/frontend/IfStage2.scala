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
  if (funcTest)
    asg(icache2.io.in.bits.isUncached, false.B)
  else
    asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  asg(icache2.io.in.bits.imask.get, io.in.bits.validMask)
  if (enableCacheInst) {
    icache2.io.cacheInst.redirect.get := io.flushIn
  }
  io.imem.ar <> icache2.dram.ar
  io.imem.r <> icache2.dram.r
  icache2.dram.aw <> DontCare
  icache2.dram.w <> DontCare
  icache2.dram.b <> DontCare
  val inBits      = io.in.bits
  val outBits     = io.out.bits
  val inValidMask = inBits.validMask
  (0 until fetchNum).foreach(i => {
    val outBasic = outBits.basicInstInfo(i)
    val inPcVal  = inBits.pcVal
    outBits.predictResult(i) := inBits.predictResult(i)
    asg(outBasic.pcVal, Cat(inPcVal(31, 5), inPcVal(4, 2) + i.U, inPcVal(1, 0)))
    asg(outBasic.instr, icache2.io.out.bits.idata.get(i)) //要求cache根据自动机状态返回全0
    outBits.validMask(i) := inBits.validMask(i) //default noBrMispre May change it
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
    val instValid = inValidMask(i) && io.in.valid //inValid
    preDecoder(i).decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    val realBrType = preDecoder(i).brType
    nonBrMisPreVec(i) := (realBrType === BranchType.NON && preTake && instValid)
    asg(io.out.bits.realBrType(i), realBrType)
  })

  /**
    * flushReg:flush下一拍，来到IF2的指令是无效的TODO:
    * out.valid:考虑到icache.out.validTODO:
    */
  val flushReg = RegNext((io.flushIn | io.noBrMispreRedirect.flush), false.B)
  when(nonBrMisPreVec.asUInt.orR && io.out.valid && !flushReg) {
    firNonBrMispre := PriorityEncoder(nonBrMisPreVec)
    val preTakeVec = WireInit(
      VecInit((0 until fetchNum).map(i => io.out.bits.predictResult(i).counter > 1.U && inValidMask(i) && io.in.valid))
    )
    val firPreTake = PriorityEncoder(preTakeVec)
    when(firNonBrMispre === firPreTake) { //FIXME:这里确实不需要flush，但是可能还是得更新BPU
      val misPc     = io.out.bits.basicInstInfo(firNonBrMispre).pcVal
      val misPreRes = io.out.bits.predictResult(firNonBrMispre)
      //mask the inst behind it
      (0 until fetchNum).map(i => { io.out.bits.validMask(i) := (i.U <= firNonBrMispre) })
      //change its preResult      ATTENTION:这里preRes无所谓改不改，因为ALU里会根据realBrType来操作
      //misPreRes.counter := 0.U  ATTENTION:这里改了会造成组合环路
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
  }
  io.bpuUpdate <> bpuUpdateQueue.io.deq

  //0707
  (0 until fetchNum).map(i => outBits.isBd(i) := false.B)

  /**
    * is延迟槽
    *   目前放在IF2段处理
    *   PRE-IF那还不太一样，如果分支没被预测跳转，是不会进入单走延迟槽的状态的
    */
  // val validBr = WireInit(
  //   VecInit((0 until fetchNum).map(i => BranchType.isBr(outBits.realBrType(i)) && outBits.validMask(i))) //outValid
  // )
  // val dsReg       = RegInit(false.B)
  // val lastDs      = WireInit(0.U((1 + log2Up(fetchNum)).W))
  // val outValidNum = PriorityCount(outBits.validMask.asUInt)
  // when(outBits.validMask(0) && io.out.valid) { dsReg := false.B }
  // when(validBr.asUInt.orR) {
  //   asg(lastDs, fetchNum.U - PriorityEncoder(validBr.reverse)) //最后一个延迟槽对应取过来的四条指令的哪一个
  //   when(lastDs >= outValidNum) {
  //     dsReg := true.B
  //   }
  // }
  // val isBd = outBits.isBd
  // (0 until (fetchNum - 1)).map(i => asg(isBd(i + 1), validBr(i)))
  // asg(isBd(0), dsReg)
  // when(io.flushIn) { dsReg := false.B }
}
