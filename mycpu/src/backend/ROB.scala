package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import frontend._
import utils.MultiQueue
import utils.asg
import chisel3.util.experimental.BoringUtils._
import difftest.DifftestPhyRegInROB
import utils.Mark

class SingleRetireBundle extends MycpuBundle {
  val muldiv = Output(Bool())
  val mtlo   = Output(Bool())
  val mthi   = Output(Bool())
  val mtc0   = Output(Bool()) //to CP0
}

class RobEntry extends MycpuBundle {
  val uOp = new RobSavedUop
  val exception = new Bundle {
    val basic  = new BasicExInfoBundle
    val detect = new DetectExInfoBundle
  }
  val isMispredict = Bool()
  val isNoBrMis    = Bool()
  val isFirPreTake = Bool()
  val done         = Bool()
  val debugPC      = if (debug) Some(UWord) else None
}

/**
  * robIndex is actually the headPtr
  *   connect to Dispatcher in Backend Module
  *     no need decouple, cause in.fromdper already decouple
  *
  * in dispatch stage
  *   rob use "robindex" to write port <fromRenameStage> into the slot
  *     pc---for difftest
  *     prevPDest---for freelist
  *     cur PA Dest---for arat
  *     special type-for special inst
  *     c0addr for mtc0
  *   rs write "robindex" into its slot
  *
  * after exeStage(or Mem2),write port <wbRob> into ROB:
  *   exception
  *   isMispredict
  *   take for ldst it's memReqVaddr,for mtxx it's wdata
  *
  * the oldest insts should retire from rob
  *     1.normal one:
  *         use multiRetire port
  *           just use <prevDestPregAddr> to push freelist
  *           hange the a-rat:
  *             use destPregAddr/destAregAddr
  *
  *      2.store:
  *         use multiRetire port
  *           can multiRetire,cause we have storeBuffer
  *           storeQ can write the result in several cycles
  *           SRAW inst(load) can request storeQ
  *
  *      3.mtc0/mthilo/muldiv:
  *            use single port
  *            mask the inst after it(for current cycle)
  *            because we only want to retire 1 this kind of inst in a cycle
  *               hilo must write retired inst at 1 cycle,and don't want to handle WAW
  *                 ---considering of potential recover(using arch-hilo)
  *               msut cp0 write in 1 cycle?
  *
  *       4.eret/exception
  *           use single port
  *           abandon itself and its little brother
  *           if not 1st,delay 1 cycle to report to cp0
  *
  *       5.mispredict:
  *           use single port
  *           just retire 1st mispre at cycle0
  *           wait for ds done
  *             ds exception:just report exception
  *             otherwise,retire ds,nect cycle mispreFlush
  */
class ROB extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromDispatcher = Vec(
        dispatchNum,
        Flipped(Decoupled(new DispatchToRobBundle))
      )
      val wbRob = Vec(wBNum, Flipped(Valid(new WbRobBundle)))

      val misPredictIdx   = Input(ROBIdx)
      val fromAluIsMisPre = Input(Bool())
    }
    val out = new Bundle {
      val robIndex  = Output(ROBIdx) //to dper
      val dsAllow   = Output(Bool()) //to dper
      val oldestIdx = Output(ROBIdx) //for ucload(LSU) and <mfc0,cacheInst(RS)>

      val singleRetire = Valid(new SingleRetireBundle) //single Retire inst
      val multiRetire = Vec(
        retireNum,
        Valid(new Bundle {
          val toArat  = new RATWriteBackIO //to arat
          val scommit = Output(Bool()) //to storeQ
          val debugPC = if (debug) Some(Output(UWord)) else None
        })
      )

      val eretFlush = Output(Bool()) //to CP0
      val exCommit  = Valid(new ExCommitBundle) //to CP0

      val flRecover          = Vec(retireNum, Valid(PRegIdx)) // FreeList recover Ports
      val mispreFlushBackend = Output(Bool()) //mispredict only FlushBackend
      val flushAll           = Output(Bool()) //serve as recover rat and hilo
      val robRedirect        = Output(new FrontRedirctIO) //serve as recover rat and hilo
    }
  })

  class ROBQueue extends MultiQueue(dispatchNum, retireNum, new RobEntry, robNum) {
    val wb = IO(
      Vec(
        wBNum,
        new Bundle {
          val wen        = Input(Bool())
          val idx        = Input(UInt())
          val exDetect   = Input(new DetectExInfoBundle)
          val misPredict = Input(Bool())
          val debugPC    = if (debug) Some(Output(UWord)) else None
        }
      )
    )
    val headIdx   = IO(Output(UInt((robIndexWidth + 1).W)))
    val tailIdx   = IO(Output(UInt((robIndexWidth + 1).W)))
    val isEmpty   = IO(Output(Bool()))
    val allPDest  = IO(Vec(robNum, Output(PRegIdx)))
    val mispreIdx = IO(Input(ROBIdx))
    val dsAllow   = IO(Output(Bool()))
    isEmpty := empty
    headIdx := headPtr
    tailIdx := tailPtr
    (0 until wBNum).foreach { i =>
      {
        if (debug) wb(i).debugPC.get := ringBuffer(wb(i).idx).debugPC.get
        when(wb(i).wen) {
          ringBuffer(wb(i).idx).done             := true.B
          ringBuffer(wb(i).idx).exception.detect := wb(i).exDetect
          ringBuffer(wb(i).idx).isMispredict     := wb(i).misPredict
        }
      }
    }
    (0 until robNum).foreach(i => {
      allPDest(i) := ringBuffer(i).uOp.currPDest
    })
    val ds = ringBuffer(mispreIdx + 1.U)
    dsAllow := (ds.done || ds.uOp.specialType =/= SpecialType.CACHEINST) && (headIdx(
      counterWidth - 1,
      0
    ) =/= mispreIdx + 1.U)

  }
  val mispreIdxReg = Module(new Mark(UInt(5.W)))
  mispreIdxReg.start.valid <> io.in.fromAluIsMisPre
  mispreIdxReg.start.bits <> io.in.misPredictIdx
  mispreIdxReg.end := io.out.flushAll || io.out.mispreFlushBackend

  val robEntries = Module(new ROBQueue)
  io.out.oldestIdx     := robEntries.io.tailPtr
  robEntries.mispreIdx := Mux(mispreIdxReg.isSet, mispreIdxReg.value.bits, io.in.misPredictIdx)
  io.out.dsAllow       := robEntries.dsAllow

  //RobEnqueue
  //Dontcare means write in WB stage
  val robEnq = robEntries.io.push
  List.tabulate(robEnq.length)(i => {
    val enqData  = robEnq(i).bits
    val fromDper = io.in.fromDispatcher(i).bits
    asg(enqData.uOp, fromDper.uOp)
    asg(enqData.exception.basic, fromDper.basicExInfo)
    asg(enqData.isNoBrMis, fromDper.isNoBrMis)
    asg(enqData.isFirPreTake, fromDper.isFirPreTake)
    asg(enqData.done, false.B)
    asg(enqData.isMispredict, false.B)
    if (debug) asg(enqData.debugPC.get, io.in.fromDispatcher(i).bits.basicExInfo.pc)
    asg(enqData.exception.detect, 0.U.asTypeOf(new DetectExInfoBundle))
    asg(robEnq(i).valid, io.in.fromDispatcher(i).valid)
    asg(io.in.fromDispatcher(i).ready, robEnq(i).ready && !robEntries.io.flush)
  })

  //WB
  val wdata = (0 until wBNum).map(i => io.in.wbRob(i).bits)
  List.tabulate(wBNum)(i => {
    robEntries.wb(i).wen        := io.in.wbRob(i).valid
    robEntries.wb(i).idx        := wdata(i).robIndex
    robEntries.wb(i).exDetect   := wdata(i).exDetect
    robEntries.wb(i).misPredict := wdata(i).isMispredict
    when(io.in.wbRob(i).valid) {
      if (debug) assert(robEntries.wb(i).debugPC.get === wdata(i).debugPC.get)
    }
  })

  //io.out.robEmpty := robEntries.isEmpty
  asg(io.out.robIndex, robEntries.headIdx(robIndexWidth - 1, 0))
  addSource(robEntries.headIdx, "ROB_HEAD_PTR")

  /**
    * retire
    *   for timing optimize,we don't want this situation happen:
    *     at certain cycle, arch-state is change ,and spec-state need to be recoverd by arch-state
    *   so:
    *     if 1st exception(且他没被某mis给abandon) not in slot 0
    *       only normal inst out at this cycle
    *       next cycle the 1st exception come to slot 0,send exception to CP0,CP0 will redirect
    *       so the outPort of exception just connect retire(0) when retire(0).slotsValid
    *     else 1st mispredict(且他没被之前的exception给abandon)
    *       ***we should notice that mispredict(and dslot) can change arch-state
    *       ***at the same time，it want to flushBackend
    *           so we just retire mispre at this cycle
    *           wait for delayslot ok
    *             if ds exeption(certainly it locate at slot0),send exception to cp0
    *             else retire it,send mispreFlush next cycle
    */

  val retireInst   = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))
  val doneMask     = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && retireInst(i).done)))
  val readyRetire  = (0 until retireNum).map(i => doneMask.asUInt(i, 0).andR)
  val retireSpType = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).uOp.specialType)))
  val retirePcVal  = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).exception.basic.pc)))
  //waitNext--msipredict | cacheinst
  val waitNextVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).isNoBrMis || retireInst(i).isMispredict || retireSpType(
          i
        ) === SpecialType.CACHEINST) && readyRetire(i)
      )
    )
  )
  //exception | eret
  val hasInt = Wire(Bool())
  addSink(hasInt, "hasInterrupt")
  val exerVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).exception.detect.happen || retireSpType(i) === SpecialType.ERET || hasInt) &&
          readyRetire(i)
      )
    )
  )
  //single retire inst
  val singleRetireVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireSpType(i) === SpecialType.MTC0 ||
          retireSpType(i) === SpecialType.MTHI ||
          retireSpType(i) === SpecialType.MTLO ||
          retireSpType(i) === SpecialType.MULDIV) &&
          readyRetire(i)
      )
    )
  )

  //sel the first mispredict and exception/eret/singleRetireInst
  val (firExEr, firWaitNext, firSingle) = (
    WireDefault(retireNum.U(log2Up(retireNum + 1).W)),
    WireDefault(retireNum.U(log2Up(retireNum + 1).W)),
    WireDefault(retireNum.U(log2Up(retireNum + 1).W))
  )
  val vecList = List(exerVec, waitNextVec, singleRetireVec)
  val firList = List(firExEr, firWaitNext, firSingle)
  List.tabulate(vecList.length)(i => {
    val vec = vecList(i)
    when(vec.asUInt.orR) {
      asg(
        firList(i),
        PriorityMux(
          (0 until retireNum).map(j => vec(j)),
          (0 until retireNum).map(_.U(log2Up(retireNum + 1).W))
        )
      )
    }
  })

  List.tabulate(retireNum)(i => { asg(io.out.multiRetire(i).valid, robEntries.io.pop(i).fire) }) //default
  val allowRobPop =
    WireInit(
      VecInit((0 until retireNum).map(i => readyRetire(i)))
    ) //default and may change,not all "readyRetire inst" can retire
  (0 until retireNum).foreach(i => { robEntries.io.pop(i).ready := allowRobPop(i) })
  asg(io.out.singleRetire.valid, false.B) //default

  //automachine
  object RetireState extends ChiselEnum {
    val normal, mpNext, misFlush, exerFlush, ciNext, exerRealFlush = Value
  }
  import RetireState._
  val hasExer     = exerVec.asUInt.orR
  val hasWaitNext = waitNextVec.asUInt.orR
  val hasSingle   = singleRetireVec.asUInt.orR
  val state       = RegInit(normal)
  import utils._
  val normalSel        = PriorityVec(VecInit(exerVec.asUInt | singleRetireVec.asUInt, waitNextVec.asUInt))
  val exerMask         = ~PriorityMask(exerVec.asUInt)
  val waitNextMask     = ~Cat(PriorityMask(waitNextVec.asUInt)(retireNum - 2, 0), 0.U(1.W))
  val singleRetireMask = ~Cat(PriorityMask(singleRetireVec.asUInt)(retireNum - 2, 0), 0.U(1.W))

  // noBr   =======================================================
  val isFirPreTakeReg = RegInit(false.B)
  val pcReg           = RegInit(0.U(32.W))
  // JMP HB =======================================================
  val findHBinRob  = RegInit(false.B)
  val dstHBFromAlu = Wire(Flipped(Valid(UWord)))
  addSink(dstHBFromAlu, "hbdest")
  val dstHB = Module(new Mark(UWord))
  dstHB.start <> dstHBFromAlu
  dstHB.end := io.out.flushAll || io.out.mispreFlushBackend
  // SC ===========================================================
  val scFailMark = Module(new Mark(UInt(0.W)))
  val scFail     = Wire(Flipped(Valid(UInt(0.W))))
  addSink(scFail, "scFail")
  scFailMark.start <> scFail
  // must first because it issue when it is oldest
  scFailMark.end := retireSpType(0) === SpecialType.STORE && io.out.multiRetire(0).fire

  // init
  io.out.eretFlush          := false.B
  io.out.exCommit.valid     := false.B
  io.out.mispreFlushBackend := false.B
  io.out.flushAll           := false.B
  addSource(io.out.flushAll, "ROB_FLUSH_ALL")
  io.out.robRedirect.flush := false.B
  // val exerFlushReg = RegInit(false.B)
  // val eretFlushReg = RegInit(false.B)
  asg(io.out.robRedirect.target, dstHB.value.bits)
  switch(state) {
    is(normal) {
      when(hasExer && firExEr <= firWaitNext && firExEr <= firSingle) {
        asg(state, exerFlush)
        asg(allowRobPop, VecInit(exerMask.asBools)) //mask itself and the inst behind
      }.elsewhen(hasWaitNext && firWaitNext <= firSingle) {
        asg(
          state,
          Mux(retireSpType(firWaitNext) === SpecialType.CACHEINST || retireInst(firWaitNext).isNoBrMis, ciNext, mpNext)
        )
        asg(allowRobPop, VecInit(waitNextMask.asBools)) //mask the inst behind
        asg(findHBinRob, retireSpType(firWaitNext) === SpecialType.HB)
        io.out.singleRetire.valid := hasSingle && (firWaitNext === firSingle) //nobrMis 指令可能是singleRetire指令
        asg(isFirPreTakeReg, retireInst(firWaitNext).isFirPreTake)
        asg(pcReg, retireInst(firWaitNext).exception.basic.pc)
      }.elsewhen(hasSingle) {
        io.out.singleRetire.valid := true.B
        asg(allowRobPop, VecInit(singleRetireMask.asBools))
      }
    }
    is(mpNext) { //MISPRE(JRHB)转移而来
      (0 until retireNum).map(i => asg(allowRobPop(i), false.B)) //default
      when(readyRetire(0)) {
        assert(retireInst(0).exception.basic.isBd)
        when(exerVec(0)) { //ds is exception
          asg(state, exerFlush)
        }.otherwise {
          asg(state, misFlush)
          allowRobPop(0)            := true.B
          io.out.singleRetire.valid := singleRetireVec(0)
        }
      }
    }
    is(ciNext) {
      (0 until retireNum).map(i => asg(allowRobPop(i), false.B))
      asg(io.out.robRedirect.target, Mux(isFirPreTakeReg, pcReg + 4.U(32.W), retirePcVal(0))) //retirePcVal(0))
      when(robEntries.io.headPtr =/= robEntries.io.tailPtr) {
        io.out.flushAll          := true.B
        io.out.robRedirect.flush := true.B
        asg(state, normal)
      }
    }
    is(misFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => asg(allowRobPop(i), false.B))
      io.out.mispreFlushBackend := true.B
      asg(io.out.robRedirect.target, dstHB.value.bits)
      when(findHBinRob) {
        io.out.flushAll          := true.B
        io.out.robRedirect.flush := true.B
        findHBinRob              := false.B
      }
    }
    is(exerFlush) {
      asg(state, exerRealFlush)
      (0 until retireNum).map(i => allowRobPop(i) := false.B)
      //io.out.flushAll := true.B
      when(retireInst(0).exception.detect.happen || hasInt) { //exception
        io.out.exCommit.valid := true.B
        val exceptType = retireInst(0).exception.detect.excCode
        when(exceptType.isOneOf(ExcCode.Sys, ExcCode.Bp, ExcCode.Tr)) {
          allowRobPop(0) := true.B
        }
      }.elsewhen(retireSpType(0) === SpecialType.ERET) { //eret
        io.out.eretFlush := true.B
        allowRobPop(0)   := true.B
      }
    }
    is(exerRealFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => asg(allowRobPop(i), false.B))
      io.out.flushAll := true.B
    }
  }
  asg(robEntries.io.flush, io.out.mispreFlushBackend || io.out.flushAll)

  //exception connect
  val oldestInst = retireInst(0)
  val oldestType = oldestInst.uOp.specialType
  val exCommit   = io.out.exCommit.bits
  // Mem badAddress =========================================
  val memReqVaddr     = Wire(UWord)
  val memException    = Wire(Bool())
  val badAddrFromMem1 = Wire(Flipped(Valid(UWord)))
  addSink(badAddrFromMem1, "mem1BadAddr")
  val badAddr = Module(new Mark(UWord))
  badAddr.start <> badAddrFromMem1
  badAddr.end  := io.out.flushAll || io.out.mispreFlushBackend
  memReqVaddr  := badAddr.value.bits
  memException := badAddr.value.valid
  asg(
    exCommit.badVaddr,
    Mux(
      (oldestType === SpecialType.LOAD || oldestType === SpecialType.STORE) && memException,
      memReqVaddr,
      oldestInst.exception.basic.pc
    )
  )
  asg(exCommit.basic, oldestInst.exception.basic)
  asg(exCommit.detect, oldestInst.exception.detect)
  when(hasInt) {
    exCommit.detect.excCode := ExcCode.Int
  }

  //multiRetire connect
  List.tabulate(retireNum)(i => {
    val retireOut = io.out.multiRetire(i).bits
    // asg(io.out.flRecover(i), retireInst(i).fromDispatcher.prevPDest)
    asg(retireOut.toArat.aDest, retireInst(i).uOp.currADest)
    asg(retireOut.toArat.pDest, retireInst(i).uOp.currPDest)
    if (i == 0) asg(retireOut.scommit, retireSpType(i) === SpecialType.STORE && !scFailMark.isSet)
    else asg(retireOut.scommit, retireSpType(i) === SpecialType.STORE)
    if (debug) asg(retireOut.debugPC.get, robEntries.io.pop(i).bits.debugPC.get)
  })

  //singleRetire connect
  val sRetireList = List(
    io.out.singleRetire.bits.mtc0,
    io.out.singleRetire.bits.mthi,
    io.out.singleRetire.bits.mtlo,
    io.out.singleRetire.bits.muldiv
  )
  (0 until sRetireList.length).map(i => asg(sRetireList(i), false.B)) //default
  val spList = List(SpecialType.MTC0, SpecialType.MTHI, SpecialType.MTLO, SpecialType.MULDIV)
  when(io.out.singleRetire.valid) {
    List.tabulate(sRetireList.length)(i => {
      asg(sRetireList(i), retireSpType(firSingle) === spList(i))
    })
  }

  // pDest collect Queue
  val mRe        = io.out.multiRetire
  val flrQueue   = RegInit(VecInit(Seq.fill(robNum)(0.U(pRegAddrWidth.W)))) //Reg(Vec(robNum, PRegIdx))
  val flrHeadPtr = RegInit(0.U((robIndexWidth + 1).W))
  val flrTailPtr = RegInit(0.U((robIndexWidth + 1).W))
  object FreeListRecover extends ChiselEnum {
    val idle, recover = Value
  }
  import FreeListRecover._
  val flrState = RegInit(FreeListRecover.idle)
  //new Version
  //if needed 1-cycle delay recover fl:
  val remainNum = flrHeadPtr - flrTailPtr
  val validPDestVec =
    WireInit(VecInit((0 until retireNum).map(i => flrQueue(dontTouch(flrTailPtr + i.U)).orR && (i.U < remainNum))))
  val pushFlNum = PopCount(validPDestVec)
  val selVec    = WireInit(VecInit.fill(retireNum)(0.U(log2Up(retireNum).W)))
  val tempValidVec =
    WireInit(VecInit.fill(retireNum)(WireInit(VecInit((0 until retireNum).map(j => validPDestVec(j))))))
  selVec(0) := PriorityEncoder(validPDestVec)
  (1 until retireNum).map(i => {
    val temp = WireInit(VecInit((0 until retireNum).map(j => tempValidVec(i - 1)(j))))
    temp(selVec(i - 1)) := false.B
    tempValidVec(i)     := (tempValidVec(i - 1).asUInt & temp.asUInt).asBools
    selVec(i)           := PriorityEncoder(tempValidVec(i))
  })
  (0 until retireNum).map { i =>
    io.out.flRecover(i).valid := i.U < pushFlNum
    io.out.flRecover(i).bits  := DontCare
    when(io.out.flRecover(i).valid) {
      io.out.flRecover(i).bits := flrQueue(dontTouch(flrTailPtr + selVec(i)))
    }
  }
  //flrstate: recover/normal

  when(flrState === recover) {
    flrTailPtr := Mux(remainNum < retireNum.U, flrTailPtr + remainNum, flrTailPtr + retireNum.U)
    val dperRdy    = flrHeadPtr - flrTailPtr < (4 * retireNum).U
    val dperRdyReg = RegInit(false.B)
    dperRdyReg := dperRdy
    (0 until dispatchNum).foreach(i => {
      io.in.fromDispatcher(i).ready := dperRdyReg
    })
    when(flrHeadPtr === flrTailPtr) {
      flrState   := idle
      dperRdyReg := true.B
    }
  }.otherwise {
    flrTailPtr := 0.U
    flrHeadPtr := 3.U
    (0 until retireNum).map(i => flrQueue(i) := Mux(mRe(i).valid, retireInst(i).uOp.prevPDest, 0.U(pRegAddrWidth.W)))
    when(robEntries.io.flush) {
      flrTailPtr := robEntries.tailIdx
      flrHeadPtr := robEntries.headIdx
      (0 until robNum).foreach(i => { flrQueue(i) := robEntries.allPDest(i) })
      flrState := recover
    }
  }

  /**
    * Old Version
    * recover fl at current cycle when normal state:
    */
  // when(flrState === recover) {
  //   val remainNum = flrHeadPtr - flrTailPtr
  //   val validPDestVec =
  //     WireInit(VecInit((0 until retireNum).map(i => flrQueue(dontTouch(flrTailPtr + i.U)).orR && (i.U < remainNum))))
  //   flrTailPtr := Mux(remainNum < retireNum.U, flrTailPtr + remainNum, flrTailPtr + retireNum.U)
  //   val pushFlNum = PopCount(validPDestVec)
  //   val selVec    = WireInit(VecInit.fill(retireNum)(0.U(log2Up(retireNum).W)))
  //   val tempValidVec =
  //     WireInit(VecInit.fill(retireNum)(WireInit(VecInit((0 until retireNum).map(j => validPDestVec(j))))))
  //   selVec(0) := PriorityEncoder(validPDestVec)

  //   (1 until retireNum).map(i => {
  //     val temp = WireInit(VecInit((0 until retireNum).map(j => tempValidVec(i - 1)(j))))
  //     temp(selVec(i - 1)) := false.B
  //     tempValidVec(i)     := (tempValidVec(i - 1).asUInt & temp.asUInt).asBools
  //     selVec(i)           := PriorityEncoder(tempValidVec(i))
  //   })
  //   // FreeList Push Valid ==========================================================
  //   (0 until retireNum).map { i =>
  //     io.out.flRecover(i).valid := i.U < pushFlNum
  //     io.out.flRecover(i).bits  := DontCare
  //     when(io.out.flRecover(i).valid) {
  //       io.out.flRecover(i).bits := flrQueue(dontTouch(flrTailPtr + selVec(i)))
  //     }
  //   }
  //   // ROB push ready ===============================================================
  //   (0 until dispatchNum).foreach(i => {
  //     io.in.fromDispatcher(i).ready := flrHeadPtr - flrTailPtr < (3 * retireNum).U
  //   })
  //   // back to normal state
  //   when(flrHeadPtr === flrTailPtr) { flrState := idle }
  // }.otherwise {
  //   // FreeList Push Valid ==========================================================
  //   val validPDestVec =
  //     WireInit(VecInit((0 until retireNum).map(i => io.out.multiRetire(i).valid && retireInst(i).uOp.prevPDest.orR)))
  //   val pushFlNum = PopCount(validPDestVec)
  //   val selVec    = WireInit(VecInit.fill(retireNum)(0.U(log2Up(retireNum).W)))
  //   val tempValidVec =
  //     WireInit(VecInit.fill(retireNum)(WireInit(VecInit((0 until retireNum).map(j => validPDestVec(j))))))
  //   selVec(0) := PriorityEncoder(validPDestVec)
  //   (1 until retireNum).map(i => {
  //     val temp = WireInit(VecInit((0 until retireNum).map(j => tempValidVec(i - 1)(j))))
  //     temp(selVec(i - 1)) := false.B
  //     tempValidVec(i)     := (tempValidVec(i - 1).asUInt & temp.asUInt).asBools
  //     selVec(i)           := PriorityEncoder(tempValidVec(i))
  //   })
  //   (0 until retireNum).map { i =>
  //     io.out.flRecover(i).valid := i.U < pushFlNum
  //     io.out.flRecover(i).bits  := DontCare
  //     when(io.out.flRecover(i).valid) {
  //       io.out.flRecover(i).bits := retireInst(selVec(i)).uOp.prevPDest
  //     }
  //   }
  // }

  //DiffTest ===================================================
  import difftest.DifftestInstrCommit
  if (verilator) {
    import chisel3.util.experimental.BoringUtils._
    val difftestRetire = Module(new DifftestInstrCommit)
    difftestRetire.io.clock := clock
    val retireInt = io.out.exCommit.valid && (io.out.exCommit.bits.detect.excCode === ExcCode.Int)
    difftestRetire.io.retireNum := Mux(retireInt, 1.U, PopCount((0 until retireNum).map(io.out.multiRetire(_).valid)))
    difftestRetire.io.lastPC := PriorityMux(
      (0 until retireNum).map(i => { io.out.multiRetire(i).valid -> retirePcVal(i) }).reverse
    )
    difftestRetire.io.interrSeq := Mux(retireInt, 0.U, retireNum.U)
    difftestRetire.io.en        := true.B
    val validRetire = Wire(Bool())
    validRetire := RegNext(difftestRetire.io.retireNum > 0.U)
    addSource(validRetire, "hasValidRetire")

    val difftestPyhROB = Module(new DifftestPhyRegInROB)
    difftestPyhROB.io.clock     := clock
    difftestPyhROB.io.en        := true.B
    difftestPyhROB.io.robHead   := robEntries.headIdx
    difftestPyhROB.io.robTail   := robEntries.tailIdx
    difftestPyhROB.io.rob       := robEntries.allPDest
    difftestPyhROB.io.flrHead   := flrHeadPtr
    difftestPyhROB.io.flrTail   := flrTailPtr
    difftestPyhROB.io.flr       := flrQueue
    difftestPyhROB.io.isRecover := flrState === recover
  }
}
