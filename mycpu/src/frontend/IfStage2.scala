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
import difftest.DifftestBCache

class BtbWIO extends MycpuBundle {
  val valid    = Vec(4, Bool())
  val tagIdx   = UInt((32 - log2Ceil(IcachLineBytes)).W)
  val instrOff = Vec(4, UInt(instrOffWidth.W))
  val brType   = Vec(fetchNum, BranchType())
  val instr    = Vec(fetchNum, UWord)
}

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
    val in        = Flipped(Decoupled(new IfStage1OutIO))
    val out       = Decoupled(new IfStage2OutIO)
    val imem      = new DramReadIO
    val btbDeq    = Decoupled(new BtbWIO)
    val selfFlush = Output(Bool())
    val dsGoIf2   = Input(Bool())
    val backFlush = Input(Bool())
    // must in this stage, becasue it use first valid btbType
    val rasPush = Valid(UWord)
    val rasPop  = Output(Bool())
    val bCacheW = Valid(new BCache.BCacheWIO)
  })
  val inBits    = io.in.bits
  val outBits   = io.out.bits
  val alignMask = inBits.alMask
  val pc        = inBits.pcVal
  val bpuout    = io.out.bits.predictResult
  val bpuSel    = inBits.bpuSel
  val bCMask    = Wire(Vec(fetchNum, Bool()))
  (0 until fetchNum).foreach(i => {
    asg(bpuout(i), inBits.bpuRes(bpuSel(i)))
    asg(bCMask(i), inBits.bCacheHit(bpuSel(i)))
  })
  val validBranch = WireInit(VecInit.fill(fetchNum)(false.B))
  val takeMask    = Wire(Vec(fetchNum, Bool()))
  (0 until fetchNum).foreach(i => {
    takeMask(i)    := inBits.bpuRes(bpuSel(i)).taken
    validBranch(i) := takeMask(i) && inBits.alMask(i)
  })
  def getByVB[T <: Data](a: Seq[T]) = {
    val res = PriorityMux(validBranch.zip(a))
    res
  }
  val predDst       = getByVB(bpuout.map(_.target))
  val hasBranch     = validBranch.asUInt.orR // make sure Priority can not be zero
  val dsFetch       = !getByVB((1 until fetchNum).map(alignMask(_)) :+ false.B) && hasBranch
  val dsMask        = getByVB(Seq("b0011".U(4.W), "b0111".U(4.W), "b1111".U(4.W), "b1111".U(4.W)))
  val bCacheHit     = getByVB(bCMask)
  val firstPredTake = VecInit(PriorityEncoderOH(validBranch))
  // >> >> >> Update RAS ==================================
  val firValidBtbType = getByVB(bpuout.map(_.btbType))
  io.rasPush.valid := firValidBtbType === BtbType.jcall && io.out.fire && hasBranch
  io.rasPop        := firValidBtbType === BtbType.jret && io.out.fire && hasBranch
  asg(io.rasPush.bits, getByVB((0 until fetchNum).map(i => Cat((pc(31, 2) + i.U + 2.U), pc(1, 0)))))

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

  val inValidMask = Mux(hasBranch, dsMask & alignMask, alignMask)
  val icache2     = Module(new CacheStage2(IcachRoads, IcachLineBytes, false, BranchType())(getIntrBrType))
  icache2.io.in.valid := io.in.valid
  io.in.ready         := icache2.io.in.ready
  asg(icache2.io.in.bits.fromStage1, io.in.bits.iCache)
  asg(icache2.io.in.bits.cancel, io.in.bits.exception =/= FrontExcCode.NONE)
  asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  asg(icache2.io.in.bits.imask.get, VecInit(inValidMask.asBools))
  val iCacheInst = Wire(Flipped(Valid(new ICacheInstIO)))
  iCacheInst.valid := false.B
  iCacheInst.bits  := DontCare
  if (enableCacheInst) {
    val robFlushAll = Wire(Bool())
    addSink(robFlushAll, "ROB_FLUSH_ALL")
    icache2.io.cacheInst.redirect.get := robFlushAll
    addSink(iCacheInst, "ICacheInstrReq")
  }
  io.imem.ar <> icache2.dram.ar
  io.imem.r <> icache2.dram.r
  icache2.dram.aw <> DontCare
  icache2.dram.w <> DontCare
  icache2.dram.b <> DontCare
  (0 until fetchNum).foreach(i => {
    val outBasic = outBits.basicInstInfo(i)
    val inPcVal  = inBits.pcVal
    asg(
      outBasic.pcVal,
      Cat(inPcVal(31, instrOffMsb + 1), inPcVal(instrOffMsb, instrOffLsb) + i.U, inPcVal(1, 0))
    )
    asg(outBasic.instr, icache2.io.out.bits.idata.get(i)) //要求cache根据自动机状态返回全0
    outBits.validMask(i) := inValidMask(i)
  })
  asg(outBits.exception, inBits.exception)
  asg(io.out.valid, icache2.io.out.valid)
  asg(icache2.io.out.ready, io.out.ready || !io.in.valid)

  //predecode
  (0 until fetchNum).foreach(i => {
    val instr = io.out.bits.basicInstInfo(i).instr

    val realBrType = icache2.io.out.bits.toUser(i)
    when(icache2.io.out.valid) {
      assert(realBrType === getIntrBrType(instr))
    }
    asg(outBits.realBrType(i), realBrType)
  })
  asg(outBits.isFirPreTake, firstPredTake)
  if (verilator) {
    val frontPreDiff = Module(new DifftestFrontPred)
    frontPreDiff.io.clock := clock
    asg(frontPreDiff.io.debugPC, VecInit(io.out.bits.basicInstInfo.map(_.pcVal)))
    asg(frontPreDiff.io.predType, VecInit(bpuout.map(_.btbType.asUInt)))
    asg(frontPreDiff.io.realType, VecInit(io.out.bits.realBrType.map(_.asUInt)))
    asg(frontPreDiff.io.en, io.out.fire)
  }

  val bpuWQ = Module(new Queue(gen = new BtbWIO, entries = 4, hasFlush = false))
  asg(bpuWQ.io.enq.valid, io.out.fire)
  asg(bpuWQ.io.enq.bits.tagIdx, outBits.basicInstInfo(0).pcVal(31, instrOffMsb + 1))
  asg(bpuWQ.io.enq.bits.instrOff, VecInit(outBits.basicInstInfo.map(_.pcVal(instrOffMsb, instrOffLsb))))
  asg(bpuWQ.io.enq.bits.brType, outBits.realBrType)
  asg(bpuWQ.io.enq.bits.valid, outBits.validMask)
  asg(bpuWQ.io.enq.bits.instr, VecInit(outBits.basicInstInfo.map(_.instr)))
  io.btbDeq <> bpuWQ.io.deq

  val normal :: waitDS :: comeDS :: Nil = Enum(3)

  val predState   = RegInit(normal)
  val savedPreDst = Reg(UWord)
  io.selfFlush := false.B
  val redirSet = io.out.bits.dsDstRedir.flush
  val redirDst = io.out.bits.dsDstRedir.target
  val alignPC  = getAlignPC(inBits.pcVal)
  redirDst := DontCare
  asg(redirSet, false.B)
  asg(outBits.isDSredir, false.B)
  val realFirstBr = PriorityEncoderOH((0 until fetchNum).map(i => outBits.realBrType(i) =/= BranchType.NON))
  asg(io.out.bits.isBd, VecInit.fill(fetchNum)(false.B))
  asg(io.bCacheW.valid, false.B)
  io.bCacheW.bits.pc  := 0.U
  io.bCacheW.bits.dst := 0.U
  switch(predState) {
    is(normal) {
      when(hasBranch && io.in.valid) {
        when(dsFetch) {
          when(inBits.bCacheDst.valid) {
            predState := waitDS
            asg(redirDst, alignPC)
            asg(outBits.isDSredir, true.B)
            asg(redirSet, true.B)
            when(io.dsGoIf2) { io.selfFlush := true.B }
            // Write BCache
            asg(io.bCacheW.valid, true.B)
            asg(io.bCacheW.bits.pc, BCache.cleanMask & inBits.pcVal)
          }.otherwise {
            // when ds come if2 in next cycle, should redirect target
            // waitDS state use instfetch signal, there use fire
            asg(redirDst, predDst)
            when(io.dsGoIf2) {
              predState := comeDS
              redirSet  := true.B
            }.otherwise {
              predState := waitDS
            }
          }
        }.otherwise {
          when(!bCacheHit) {
            asg(redirDst, predDst)
            asg(redirSet, true.B)
            when(io.dsGoIf2) { io.selfFlush := true.B }
            // Write BCache
            asg(io.bCacheW.valid, true.B)
            asg(io.bCacheW.bits.pc, inBits.pcVal)
            asg(io.bCacheW.bits.dst, predDst)
          }
        }
        asg(savedPreDst, predDst)
      }.elsewhen(io.in.valid) {
        when(inBits.bCacheDst.valid) {
          asg(redirDst, alignPC)
          asg(redirSet, true.B)
          when(io.dsGoIf2) { io.selfFlush := true.B }
        }
      }
    }
    is(waitDS) {
      asg(redirDst, savedPreDst)
      when(io.dsGoIf2) {
        predState := comeDS
        asg(redirSet, true.B)
      }
    }
    is(comeDS) {
      io.out.bits.predictResult(0).btbType := BtbType.non
      io.out.bits.predictResult(0).counter := 0.U
      io.out.bits.predictResult(0).taken   := false.B
      asg(io.out.bits.validMask, VecInit(Cat(Fill(fetchNum - 1, false.B), true.B).asBools))
      when(io.out.fire) { predState := normal }
    }
  }
  when(io.backFlush) { predState := normal }
  if (enableCacheInst) {
    when(iCacheInst.valid) { predState := normal }
  }
  if (verilator) {
    val diffBCache = Module(new DifftestBCache)
    asg(diffBCache.io.clock, clock)
    asg(diffBCache.io.en, true.B)
    val if2FireIn = Wire(Bool())
    addSink(if2FireIn, "If2FireIn")
    asg(diffBCache.io.fireIn, RegNext(if2FireIn))
    when(diffBCache.io.fireIn) {
      assert(io.in.valid)
    }
    asg(diffBCache.io.state, predState)
    asg(diffBCache.io.bCacheDst, inBits.bCacheDst.bits)
    asg(diffBCache.io.predictDst, predDst)
    asg(diffBCache.io.bCacheHit, bCacheHit)
    asg(diffBCache.io.wDst, io.bCacheW.bits.dst)
    asg(diffBCache.io.wPC, io.bCacheW.bits.pc)
    asg(diffBCache.io.wen, io.bCacheW.valid)
    asg(diffBCache.io.pcVal, inBits.pcVal)
    asg(diffBCache.io.bCacheUse, inBits.bCacheDst.valid)
    asg(diffBCache.io.dsFetch, dsFetch)
    asg(diffBCache.io.hasBranch, hasBranch)
  }
}
