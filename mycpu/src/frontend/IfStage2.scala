package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import cache._
import decodemacro.MacroDecode
import chisel3.util.experimental.BoringUtils._
import difftest.DifftestFrontPred

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
class IfStage2 extends MycpuModule {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new IfStage1OutIO))
    val out  = Decoupled(new IfStage2OutIO)
    val imem = new DramReadIO

    val noBrMispreRedirect = new FrontRedirctIO
    val bpuUpdate          = Decoupled(new BpuUpdateIO)
  })
  def getIntrBrType(instr: UInt): BranchType.Type = {
    @MacroDecode
    class IF2PreDecodeOut extends MycpuBundle {
      val brType = BranchType.NON
    }
    import chisel3.util.experimental.decode.QMCMinimizer
    require(instr.getWidth == 32)
    val foo = Wire(new IF2PreDecodeOut)
    foo.decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    foo.brType
  }
  val icache2 = Module(new CacheStage2(IcachRoads, IcachLineBytes, false, BranchType())(getIntrBrType))
  icache2.io.in.valid := io.in.valid
  io.in.ready         := icache2.io.in.ready
  asg(icache2.io.in.bits.fromStage1, io.in.bits.iCache)
  asg(icache2.io.in.bits.cancel, io.in.bits.exception =/= FrontExcCode.NONE)
  asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  asg(icache2.io.in.bits.imask.get, io.in.bits.validMask)
  if (enableCacheInst) {
    val robFlushAll = Wire(Bool())
    addSink(robFlushAll, "ROB_FLUSH_ALL")
    icache2.io.cacheInst.redirect.get := robFlushAll
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
    asg(
      outBasic.pcVal,
      Cat(inPcVal(31, instrOffMsb + 1), inPcVal(instrOffMsb, instrOffLsb) + i.U, inPcVal(1, 0))
    )
    asg(outBasic.instr, icache2.io.out.bits.idata.get(i)) //要求cache根据自动机状态返回全0
    outBits.validMask(i) := inBits.validMask(i) //default noBrMispre May change it
  })
  asg(outBits.exception, inBits.exception)
  asg(io.out.valid, icache2.io.out.valid)
  asg(icache2.io.out.ready, io.out.ready)

  //default
  asg(io.noBrMispreRedirect.flush, false.B)
  asg(io.noBrMispreRedirect.target, 0.U(vaddrWidth.W))
  io.bpuUpdate       := DontCare
  io.bpuUpdate.valid := false.B

  //predecode
  (0 until fetchNum).foreach(i => {
    val instr = io.out.bits.basicInstInfo(i).instr

    val instValid  = inValidMask(i) && io.in.valid //inValid
    val realBrType = icache2.io.out.bits.toUser(i)
    when(icache2.io.out.valid) {
      if (sim) assert(realBrType === getIntrBrType(instr))
    }
    asg(io.out.bits.realBrType(i), realBrType)
  })

  //fir preTake
  val preTakeVec = WireInit(
    VecInit((0 until fetchNum).map(i => io.out.bits.predictResult(i).taken && inValidMask(i) && io.in.valid))
  )
  val firPreTake = PriorityEncoder(preTakeVec)
  (0 until fetchNum).map(i => outBits.isFirPreTake(i) := false.B) //default
  when(preTakeVec.asUInt.orR) {
    outBits.isFirPreTake(firPreTake) := true.B
  }

  //isBd
  (0 until fetchNum).map(i => outBits.isBd(i) := false.B)
  if (verilator) {
    val frontPreDiff = Module(new DifftestFrontPred)
    frontPreDiff.io.clock := clock
    asg(frontPreDiff.io.debugPC, VecInit(io.out.bits.basicInstInfo.map(_.pcVal)))
    asg(frontPreDiff.io.predType, VecInit(io.in.bits.predictResult.map(_.btbType.asUInt)))
    asg(frontPreDiff.io.realType, VecInit(io.out.bits.realBrType.map(_.asUInt)))
    asg(frontPreDiff.io.en, io.out.fire)
  }
}
