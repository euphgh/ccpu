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
import utils._

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
      val fromDispatcher  = Vec(dispatchNum, Flipped(Decoupled(new DispatchToRobBundle)))
      val wbRob           = Vec(wBNum, Flipped(Valid(new WbRobBundle)))
      val misPredictIdx   = Input(ROBIdx)
      val fromAluIsMisPre = Input(Bool())
    }
    val out = new Bundle {
      val robIndex  = Output(ROBIdx) //to dper
      val dsAllow   = Output(Bool()) //to dper
      val oldestIdx = Output(ROBIdx) //for block inst
      //retire port
      val singleRetire = Valid(new SingleRetireBundle)
      val multiRetire = Vec(
        retireNum,
        Valid(new Bundle {
          val toArat  = new RATWriteBackIO
          val scommit = Output(Bool())
          val debugPC = if (debug) Some(Output(UWord)) else None
        })
      )
      //to CP0
      val eretFlush = Output(Bool())
      val exCommit  = Valid(new ExCommitBundle)
      //recover|flush|redirect
      val flRecover          = Vec(retireNum, Valid(PRegIdx)) // FreeList recover Ports
      val mispreFlushBackend = Output(Bool()) //mispredict only FlushBackend
      val flushAll           = Output(Bool()) //serve as recover rat and hilo
      val robRedirect        = Output(new FrontRedirctIO) //serve as recover rat and hilo
    }
  })
  if (debug) {
    val instrNum      = RegInit(0.U(32.W))
    val multCommitVec = VecInit(io.out.multiRetire.map(_.fire)).asUInt
    when(multCommitVec.orR) { dontTouch(instrNum) := instrNum + PopCount(multCommitVec) }
  }

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

  asg(io.out.robIndex, robEntries.headIdx(robIndexWidth - 1, 0))
  addSource(robEntries.headIdx, "ROB_HEAD_PTR")

  /**
    * for timing optimize:
    *     flush and retire can't be in the same cycle
    *       ds retire -> misFlush
    *       normal retire -> exerFlush
    *     use reg for mulReV SingleReV redirectTarget
    *     only use doneVec and normalState to gen robPop
    *     for FL:
    *         delay 1 cycle to push when normal
    *         use reg for dperToRob_Rdy
    *
    * 2 stage retire:
    *   stage1:pop from ROB
    *   stage2:real Retire
    * we design automachine for stage2,to handle special situation
    *
    * robPop =/= retireRegUpdate
    * robPop =/= retire
    * some misRoad may be pop(certainly not retire)
    *       ATTENTION OF FLRQUEUE
    *
    * single can reuse most of normalState logic
    *   should care about retireReg update(right shift)
    */

  //automachine
  object RetireState extends ChiselEnum {
    val normal, single, preExEr, exEr, exerFlush, preNext, mpNext, dsRetire, misFlush, ciNext, ciFlush = Value
  }
  import RetireState._
  val state  = RegInit(normal)
  val firGo  = "b001".U
  val noneGo = "b000".U
  //retire
  val retireReg    = RegInit(VecInit.fill(retireNum)(0.U.asTypeOf(new RobEntry)))
  val retirePcVal  = WireInit(VecInit((0 until retireNum).map(i => retireReg(i).exception.basic.pc)))
  val retireSpType = WireInit(VecInit((0 until retireNum).map(i => retireReg(i).uOp.specialType)))
  //robPop
  val robPopInst   = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))
  val ropPopSpType = WireInit(VecInit((0 until retireNum).map(i => robPopInst(i).uOp.specialType)))
  val doneVec      = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && robPopInst(i).done)))
  val robRdyGo     = WireInit(VecInit((0 until retireNum).map(i => doneVec.asUInt(i, 0).andR))) //already mask
  //mispre|ci
  val waitNextVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (robPopInst(i).isMispredict ||
          robPopInst(i).isNoBrMis ||
          ropPopSpType(i) === SpecialType.CACHEINST ||
          ropPopSpType(i) === SpecialType.TLB ||
          ropPopSpType(i) === SpecialType.MTC0) && robRdyGo(i)
      )
    )
  )
  //exception | eret
  val hasInt = Wire(Bool())
  addSink(hasInt, "hasInterrupt")
  val exerVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (robPopInst(i).exception.detect.happen || ropPopSpType(i) === SpecialType.ERET || hasInt) &&
          robRdyGo(i)
      )
    )
  )
  //single retire
  val sRetireVec = WireInit(VecInit((0 until retireNum).map(i => robPopInst(i).uOp.isSingle && robRdyGo(i))))

  //get fir
  def getFir(vec: Vec[Bool]) = {
    val len = retireNum
    val fir = WireDefault(len.U(log2Up(len + 1).W))
    when(vec.asUInt.orR) {
      asg(
        fir,
        PriorityMux(
          (0 until len).map(j => vec(j)),
          (0 until len).map(_.U(log2Up(len + 1).W))
        )
      )
    }
    fir
  }
  //sel the first exer|wn|single
  val (hasExer, hasWaitNext, hasSingle) = (exerVec.asUInt.orR, waitNextVec.asUInt.orR, sRetireVec.asUInt.orR)
  val (firExEr, firWaitNext, firSingle) = (getFir(exerVec), getFir(waitNextVec), getFir(sRetireVec))
  val vecList                           = List(exerVec, waitNextVec, sRetireVec, doneVec)

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
  scFailMark.start <> scFail
  addSink(scFail, "scFail")
  scFailMark.end := retireSpType(0) === SpecialType.STORE && io.out.multiRetire(0).fire //it issue when it's oldest
  val letLLGoState = WireInit(state === normal | state === mpNext)
  addSource(letLLGoState, "llgoState") //for ll

  // variable
  val nextInRobReg    = RegInit(false.B)
  val dsPopReg        = RegInit(false.B)
  val sReVReg         = RegInit(false.B)
  val multiReVReg     = RegInit(0.U(retireNum.W))
  val ciRediTargetReg = RegInit(0.U(vaddrWidth.W))
  val firIdxReg       = RegInit(retireNum.U(log2Up(retireNum + 1).W))
  //for single state
  val shiftExEr = RegInit(VecInit.fill(retireNum)(false.B))
  val shiftWn   = RegInit(VecInit.fill(retireNum)(false.B))
  val shiftSVec = RegInit(VecInit.fill(retireNum)(false.B))
  val shiftDone = RegInit(VecInit.fill(retireNum)(false.B))
  val shiftList = List(shiftExEr, shiftWn, shiftSVec, shiftDone)
  // init
  io.out.flushAll           := false.B
  io.out.eretFlush          := false.B
  io.out.exCommit.valid     := false.B
  io.out.robRedirect.flush  := false.B
  io.out.mispreFlushBackend := false.B
  val allowRobPop = WireInit(VecInit((0 until retireNum).map(i => robRdyGo(i) && state === normal)))
  //connect
  io.out.singleRetire.valid := sReVReg
  io.out.robRedirect.target := Mux(state === misFlush, dstHB.value.bits, ciRediTargetReg)
  robEntries.io.flush       := io.out.mispreFlushBackend || io.out.flushAll
  addSource(io.out.flushAll, "ROB_FLUSH_ALL")
  (0 until retireNum).foreach(i => {
    io.out.multiRetire(i).valid := multiReVReg(i)
    robEntries.io.pop(i).ready  := allowRobPop(i)
  })

  /**
    * dsRetiring means that ds (notEdge & done & !exer & !single)
    *   noneGo notSrev misFlush
    * else if exer(not matter edge or not)->exer
    * else if isSingle(not matter edge or not)->dsRetire firGo SreV
    * else if done->dsRetire firGo notSrev
    * else(not done)->mpnext noneGo notSrev
    */
  def dealDs(ds: RobEntry, dsRetiring: Bool, dsPoped: Bool, hasInt: Bool) = {
    val exType    = ds.exception.detect.excCode
    val dsDone    = Mux(dsPoped, ds.done, doneVec(0))
    val isEx      = (ds.exception.detect.happen | hasInt) & dsDone
    val isEr      = ds.uOp.specialType === SpecialType.ERET & dsDone
    val isExEr    = isEr | isEx
    val isSingle  = ds.uOp.isSingle & dsDone
    val exErMulRe = Mux(isEr | ExcCode.canRe(exType), firGo, noneGo)
    val sReV      = !isExEr & isSingle
    val mulReV    = Mux(dsRetiring, noneGo, Mux(isExEr, exErMulRe, Mux(isSingle | dsDone, firGo, noneGo)))
    val nextState = Mux(dsRetiring, misFlush, Mux(isExEr, exEr, Mux(isSingle | dsDone, dsRetire, mpNext)))
    (nextState, mulReV, sReV)
  }

  /**
    * for normal and single state
    * gen nextstate mrev srev firIdxReg
    */
  def dealNormal(
    exerVec: Vec[Bool],
    wnVec:   Vec[Bool],
    sVec:    Vec[Bool],
    doneVec: Vec[Bool],
    isMis:   Bool
  ) = {
    //get Fir
    val (firExEr, firWaitNext, firSingle) = (getFir(exerVec), getFir(wnVec), getFir(sVec))
    val (hasExer, hasWaitNext, hasSingle) = (exerVec.asUInt.orR, wnVec.asUInt.orR, sVec.asUInt.orR)
    //get mask
    val exerMask = ~PriorityMask(exerVec.asUInt) //mask inst include itself
    val ciMask   = ~Cat(PriorityMask(wnVec.asUInt)(retireNum - 2, 0), 0.U(1.W)) //mask the inst behind
    val sReMask  = ~Cat(PriorityMask(sVec.asUInt)(retireNum - 2, 0), 0.U(1.W)) //mask the inst behind
    val mpMask   = Wire(Vec(retireNum, Bool()))
    if (retireNum < 3) { (0 until retireNum).map(i => { mpMask(i) := true.B }) }
    else { mpMask := (~Cat(PriorityMask(wnVec.asUInt)(retireNum - 3, 0), 0.U(2.W))).asBools } //mask ds behind
    when(firWaitNext < (retireNum - 1).U) {
      val dsIdx = firWaitNext + 1.U
      mpMask(dsIdx) := doneVec(dsIdx) & !exerVec(dsIdx) & !sVec(dsIdx)
    }
    //init
    val nextState = WireInit(normal)
    val sReV      = WireInit(false.B)
    val firIdx    = WireInit(firSingle)
    val mReV      = WireInit(VecInit((0 until retireNum).map(i => doneVec.asUInt(i, 0).andR)).asUInt)
    //gen state mReV sReV firIdx
    when(hasExer && firExEr <= firWaitNext && firExEr <= firSingle) {
      asg(nextState, preExEr)
      sReV   := false.B
      mReV   := exerMask
      firIdx := firExEr
    }.elsewhen(hasWaitNext && firWaitNext <= firSingle) {
      asg(nextState, preNext)
      sReV   := hasSingle && (firWaitNext === firSingle)
      mReV   := Mux(isMis, mpMask.asUInt, ciMask)
      firIdx := firWaitNext
    }.elsewhen(hasSingle) {
      asg(nextState, single)
      sReV   := true.B
      mReV   := sReMask
      firIdx := firSingle
    }
    (nextState, mReV, sReV, firIdx)
  }
  //state transition
  switch(state) {
    is(normal) {
      val isMis = WireInit(false.B)
      when(hasWaitNext) {
        nextInRobReg := (robEntries.io.headPtr =/= robEntries.io.tailPtr + firWaitNext + 1.U)
        findHBinRob  := ropPopSpType(firWaitNext) === SpecialType.HB
        isMis        := robPopInst(firWaitNext).isMispredict
      }
      when(hasSingle && firSingle < firWaitNext && firSingle < firExEr) {
        val sSize = firSingle + 1.U
        (0 until shiftList.length).map(i => shiftList(i) := (vecList(i).asUInt >> sSize).asBools)
      }
      val deal = dealNormal(exerVec, waitNextVec, sRetireVec, doneVec, isMis)
      state       := deal._1
      multiReVReg := deal._2
      sReVReg     := deal._3
      firIdxReg   := deal._4
    }
    //retire normal->retire exer->flush
    is(preExEr) {
      asg(state, exEr)
      val exType = retireReg(firIdxReg).exception.detect.excCode
      multiReVReg := noneGo
      sReVReg     := false.B
      when(ExcCode.canRe(exType) || retireSpType(firIdxReg) === SpecialType.ERET) {
        multiReVReg := firGo
      }
      firIdxReg    := 0.U
      retireReg(0) := retireReg(firIdxReg)
    }
    is(exEr) {
      asg(state, exerFlush)
      when(retireReg(0).exception.detect.happen || hasInt) { //exception
        io.out.exCommit.valid := true.B
      }.elsewhen(retireSpType(0) === SpecialType.ERET) { //eret
        io.out.eretFlush := true.B
      }
    }
    is(exerFlush) {
      asg(state, normal)
      io.out.flushAll := true.B
    }

    /**
      * we get mulReV/sReV in prev state(normal):
      *   for ci:mask inst behind
      *   for mp:notDone|isSingle|ExEr|edge Ds can't retire
      * we generate next mulRev/sRev/stage:
      *   for ci:false,stage depend on nextInRob
      *   for mp:see dealDs,dsPopReg is for FL
      *     note that the param ds is just ds:
      *       edge:robPop(0)
      *       notEdge:depend on retireReg(mpIdx+1)
      *         if done(doneVec Reg):indicate it's a exer|single ds
      *         else:ds not Pop yet
      */
    is(preNext) {
      //for mp
      val ds                        = WireInit(0.U.asTypeOf(new RobEntry))
      val dsRetiring                = WireInit(false.B)
      val dsPoped                   = WireInit(false.B)
      val deal                      = dealDs(ds, dsRetiring, dsPoped, hasInt = false.B) //hasInt=hasInt
      val isMis                     = retireReg(firIdxReg).isMispredict
      val (mpState, mpMulRe, mpSRe) = (deal._1, deal._2, deal._3)
      //for ci
      val nextEntryPc = WireInit(robPopInst(0).exception.basic.pc)
      val firPreTake  = retireReg(firIdxReg).isFirPreTake
      val nextInRob   = robEntries.io.headPtr =/= robEntries.io.tailPtr
      val ciState     = Mux(firPreTake | nextInRob | nextInRobReg, ciFlush, ciNext)
      //coding
      when(firIdxReg === (retireNum - 1).U) {
        ds := robPopInst(0)
      }.otherwise {
        val mayDs = retireReg(firIdxReg + 1.U)
        ds         := Mux(mayDs.done, mayDs, robPopInst(0))
        dsPoped    := mayDs.done
        dsRetiring := multiReVReg(firIdxReg + 1.U)
        when(nextInRobReg) { nextEntryPc := retirePcVal(firIdxReg + 1.U) }
      }
      //state transition and redirect
      state           := Mux(isMis, mpState, ciState)
      multiReVReg     := Mux(isMis, mpMulRe, noneGo)
      sReVReg         := Mux(isMis, mpSRe, false.B)
      ciRediTargetReg := Mux(firPreTake, retirePcVal(firIdxReg) + 4.U(32.W), nextEntryPc)
      //use port_0 to retire
      when(isMis) {
        retireReg(0) := ds
        firIdxReg    := 0.U
      }
      dsPopReg := dsPoped
    }
    //come here when ds not done,trans to dsRetire or exer when dsDone
    is(mpNext) {
      val ds   = robPopInst(0)
      val deal = dealDs(ds, dsRetiring = false.B, dsPoped = false.B, hasInt = false.B) //hasInt=hasInt
      when(robRdyGo(0)) { assert(ds.exception.basic.isBd) }
      state        := deal._1
      sReVReg      := deal._3
      multiReVReg  := deal._2
      firIdxReg    := 0.U //use port_0 to retire
      retireReg(0) := ds
    }
    is(dsRetire) {
      asg(state, misFlush)
    }
    is(misFlush) {
      asg(state, normal)
      io.out.mispreFlushBackend := true.B
      when(findHBinRob) {
        io.out.flushAll          := true.B
        io.out.robRedirect.flush := true.B
        findHBinRob              := false.B
      }
    }
    //!(isFirPreTake or nextInRob) come to this state,wait for ci in Rob
    is(ciNext) {
      ciRediTargetReg := robPopInst(0).exception.basic.pc
      when(robEntries.io.headPtr =/= robEntries.io.tailPtr) {
        asg(state, ciFlush)
      }
    }
    is(ciFlush) {
      io.out.flushAll          := true.B
      io.out.robRedirect.flush := true.B
      asg(state, normal)
    }

    /**
      * to simplify rob pop logic
      *   just use robRdyGo and (state===normal) to gen pop_rdy
      *   so other inst may pop from rob when single<(exer|wn)
      * use a single state to handle
      *   it retire firSingle and before
      *   should gen next mulRe sRe state depend on the left inst
      *   can reuse mostly normalState logic
      */
    is(single) {
      //shift inst in retireReg
      (0 until retireNum).map(i => {
        when(i.U > firIdxReg) { retireReg(i.U - firIdxReg - 1.U) := retireReg(i) }
        when(i.U >= (retireNum.U - firIdxReg - 1.U)) { retireReg(i.U).done := false.B }
      })
      //use shift vec
      val (fExEr, fWaitNext, fSingle) = (getFir(shiftExEr), getFir(shiftWn), getFir(shiftSVec))
      val (hExer, hWaitNext, hSingle) = (shiftExEr.asUInt.orR, shiftWn.asUInt.orR, shiftSVec.asUInt.orR)
      //prepare for special
      val isMis = WireInit(false.B)
      when(hWaitNext) {
        val idx = firIdxReg + fWaitNext + 1.U
        isMis := retireReg(idx).isMispredict
      }
      when(hSingle && fSingle < fWaitNext && fSingle < fExEr) {
        val sSize = fSingle + 1.U
        (0 until shiftList.length).map(i => shiftList(i) := (shiftList(i).asUInt >> sSize).asBools)
      }
      //gen nextState mrev srev firIdx
      val deal = dealNormal(shiftExEr, shiftWn, shiftSVec, shiftDone, isMis)
      state       := deal._1
      multiReVReg := deal._2
      sReVReg     := deal._3
      firIdxReg   := deal._4
    }
  }

  //these 4 state have their own logic of generating retireVReg
  when(state =/= normal & state =/= preNext & state =/= mpNext & state =/= preExEr & state =/= single) {
    multiReVReg := noneGo
    sReVReg     := false.B
  }
  //these 3 state can't update like usual,see update logic above
  when(state =/= preExEr & state =/= preNext & state =/= mpNext & state =/= single) {
    (0 until retireNum).map(i => {
      retireReg(i)      := robPopInst(i)
      retireReg(i).done := doneVec(i)
    })
  }

  //exception connect
  val exerInst = retireReg(0)
  val exerSpT  = exerInst.uOp.specialType
  val exCommit = io.out.exCommit.bits
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
      (exerSpT === SpecialType.LOAD || exerSpT === SpecialType.STORE) && memException,
      memReqVaddr,
      exerInst.exception.basic.pc
    )
  )
  asg(exCommit.basic, exerInst.exception.basic)
  asg(exCommit.detect, exerInst.exception.detect)
  when(hasInt) {
    exCommit.detect.excCode := ExcCode.Int
  }
  //multiRetire connect
  List.tabulate(retireNum)(i => {
    val mulRe = io.out.multiRetire(i).bits
    asg(mulRe.toArat.aDest, retireReg(i).uOp.currADest)
    asg(mulRe.toArat.pDest, retireReg(i).uOp.currPDest)
    if (i == 0) asg(mulRe.scommit, retireSpType(i) === SpecialType.STORE && !scFailMark.isSet)
    else asg(mulRe.scommit, retireSpType(i) === SpecialType.STORE)
    if (debug) { asg(mulRe.debugPC.get, retireReg(i).debugPC.get) }
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
      asg(sRetireList(i), retireSpType(firIdxReg) === spList(i))
    })
  }

  /**
    * FreeList Recover:
    *   dperRdy use Reg
    *   simple(now)
    *     normal:retire(enqEn) -> pushFl_en -> pushFl_In
    *     recover:flush(enqEn) -> pushFl_en -> pushFl_In
    *     "pushValid(and Bits) is too long"
    *   ATTENTION:
    *     now wrongRoad inst may pop from rob(certainly not retire)
    *       so find a way to recover these inst's PDest
    *     and mp_ds may not pop from rob
    *       use a reg to mark
    *       when misFlush,should pay attention to flrQ_tailPtr
    *
    *   Todo(V1)
    *     normal:retire(enqEn and calV) -> pushFl_en -> pushFl_In
    *     recover:flush(enqEn and calV) -> pushFl_en -> pushFl_In
    */
  val mRe        = io.out.multiRetire
  val flrQueue   = RegInit(VecInit(Seq.fill(robNum)(0.U(pRegAddrWidth.W)))) //Reg(Vec(robNum, PRegIdx))
  val flrHeadPtr = RegInit(0.U((robIndexWidth + 1).W))
  val flrTailPtr = RegInit(0.U((robIndexWidth + 1).W))
  object FreeListRecover extends ChiselEnum {
    val idle, recover = Value
  }
  import FreeListRecover._
  val flrState = RegInit(FreeListRecover.idle)

  /**
    * new Version
    *   use reg for freelist push valid and bits
    */
  // val pushVReg   = RegInit(VecInit.fill(retireNum)(false.B))
  // val pushBReg   = RegInit(VecInit.fill(retireNum)(0.U(pRegAddrWidth.W)))
  // val dperRdyReg = RegInit(false.B)
  // val flRe       = io.out.flRecover
  // (0 until retireNum).map(i => {
  //   flRe(i).valid := pushVReg(i)
  //   flRe(i).bits  := pushBReg(i)
  // })

  // /**
  //   * given a vec of bits(need to be all_0 when unvalid)
  //   * gen a pushV and pushB for next cycle
  //   *   only not zero bits is valid
  //   */
  // def getPushFlVB(bits: Vec[UInt]) = {
  //   val validVec  = WireInit(VecInit((0 until retireNum).map(i => bits(i).orR)))
  //   val pushFlNum = PopCount(validVec)
  //   val selVec    = WireInit(VecInit.fill(retireNum)(0.U(log2Up(retireNum + 1).W)))
  //   val pushV     = WireInit(VecInit((0 until retireNum).map(i => i.U < pushFlNum)))
  //   val pushB     = WireInit(VecInit.fill(retireNum)(0.U(pRegAddrWidth.W)))
  //   val tempValidVec =
  //     WireInit(VecInit.fill(retireNum)(WireInit(VecInit((0 until retireNum).map(j => validVec(j))))))
  //   selVec(0) := PriorityEncoder(validVec)
  //   (1 until retireNum).map(i => {
  //     val temp = WireInit(VecInit((0 until retireNum).map(j => tempValidVec(i - 1)(j))))
  //     temp(selVec(i - 1)) := false.B
  //     tempValidVec(i)     := (tempValidVec(i - 1).asUInt & temp.asUInt).asBools
  //     selVec(i)           := PriorityEncoder(tempValidVec(i))
  //   })
  //   (0 until retireNum).map(i => {
  //     when(pushV(i)) {
  //       pushB(i) := bits(selVec(i))
  //     }
  //   })
  //   (pushB, pushV)
  // }

  // /** flr normal state
  //   * should handle special situation in robState:(preExEr and preNext)
  //   *   wrong road inst will be poped(certainly not retire them)if its leading done==true
  //   *   when flush,these inst'Pdest can't get in flQueue,so need to recover them in these 2 states
  //   *   wrong inst include:
  //   *     preExer:exerAndBehind
  //   *     preNext:
  //   *       ci:behind ci
  //   *       mis:if ds exer dsAndBehind,else behindDs(note that there is no dsWithInt in preNext)
  //   */
  // val norP = WireInit(
  //   VecInit(
  //     (0 until retireNum).map(i => Mux(mRe(i).valid & state =/= exEr, retireReg(i).uOp.prevPDest, 0.U(pRegAddrWidth.W)))
  //   )
  // )
  // val getNorVB = getPushFlVB(norP) //when flush, norP is the tail of rob.allPdest
  // when(flrState === idle) {
  //   val doneVec = WireInit(VecInit((0 until retireNum).map(i => retireReg(i).done)))
  //   val misIdx  = WireInit(2.U)
  //   when(firIdxReg + 1.U =/= retireNum.U) {
  //     val ds = retireReg(firIdxReg + 1.U)
  //     when(ds.done && (ds.exception.detect.happen | ds.uOp.specialType === SpecialType.ERET)) {
  //       misIdx := 1.U
  //     }
  //   }
  //   val idx =
  //     Mux(state === preExEr, 0.U, Mux(retireReg(firIdxReg).isMispredict, misIdx, 1.U))
  //   when(state === preExEr | state === preNext) {
  //     (0 until retireNum).map(i => {
  //       when(i.U >= firIdxReg && (i.U - firIdxReg) >= idx) {
  //         val mask = !doneVec.asUInt(i, 0).andR
  //         norP(i) := Mux(mask, 0.U(pRegAddrWidth.W), retireReg(i).uOp.currPDest)
  //       }
  //     })
  //   }
  //   pushBReg := getNorVB._1
  //   pushVReg := getNorVB._2
  // }

  // /**
  //   * flush
  //   * not poped ds is in rob when misflush
  //   *   we can't let it get into flrQ(it has pushed its prevPDest in preNext or dsRetire state)
  //   *   note that -> misflush only when ds not exEr
  //   *     otherwise,it will ->exer->exerflush(ds get into flrQ and push its curPDest)
  //   * So,when misFlush,be aware of rob.tailIdx
  //   *
  //   * recover state
  //   * t0:flush
  //   *   use rob tail to cal next cycle pushB/pushV
  //   *   cal flrQ Ptr and dperRdyReg
  //   *   cal flrstate
  //   * t1:recover state
  //   *   use flrQueue tail to gen next cycle pushB/pushV
  //   *   the cycle flrQueue empty,flrstate back to normal
  //   */
  // when(robEntries.io.flush) {
  //   assert(!multiReVReg.asUInt.orR)
  //   val robRealTIdx = WireInit(robEntries.tailIdx)
  //   when(state === misFlush & !dsPopReg) { robRealTIdx := robEntries.tailIdx + 1.U }
  //   val robLeftNum = robEntries.headIdx - robRealTIdx
  //   dperRdyReg := robLeftNum <= ((6 * retireNum).U)
  //   flrTailPtr := Mux(robLeftNum < retireNum.U, robEntries.headIdx, robRealTIdx + retireNum.U)
  //   flrHeadPtr := robEntries.headIdx
  //   (0 until robNum).foreach(i => { flrQueue(i) := robEntries.allPDest(i) })
  //   when(robLeftNum > retireNum.U) { flrState := recover }
  //   (0 until retireNum).map(i =>
  //     norP(i) := Mux(i.U < robLeftNum, robEntries.allPDest(dontTouch(i.U + robRealTIdx)), 0.U(pRegAddrWidth.W))
  //   )
  // }
  // when(flrState === recover) {
  //   assert(!robEntries.io.flush && !multiReVReg.asUInt.orR)
  //   val flrRemainNum   = flrHeadPtr - flrTailPtr
  //   val nextFlrTailPtr = Mux(flrRemainNum < retireNum.U, flrTailPtr + flrRemainNum, flrTailPtr + retireNum.U)
  //   flrTailPtr := nextFlrTailPtr
  //   dperRdyReg := flrRemainNum <= ((6 * retireNum).U)
  //   (0 until dispatchNum).foreach(i => { io.in.fromDispatcher(i).ready := dperRdyReg })
  //   when(flrHeadPtr === nextFlrTailPtr) { flrState := idle }
  //   val recoverP = WireInit(
  //     VecInit(
  //       (0 until retireNum).map(i =>
  //         Mux(i.U < flrRemainNum, flrQueue(dontTouch(flrTailPtr + i.U)), 0.U(pRegAddrWidth.W))
  //       )
  //     )
  //   )
  //   val getRecoverVB = getPushFlVB(recoverP)
  //   pushBReg := getRecoverVB._1
  //   pushVReg := getRecoverVB._2
  // }

  //old Version
  //if needed 1-cycle delay recover fl:
  val remainNum = flrHeadPtr - flrTailPtr
  val validPDestVec =
    WireInit(VecInit((0 until retireNum).map(i => flrQueue(dontTouch(flrTailPtr + i.U)).orR && (i.U < remainNum))))
  val pushFlNum = PopCount(validPDestVec)
  val selVec    = WireInit(VecInit.fill(retireNum)(0.U(log2Up(retireNum).W)))
  //get selVec
  val tempValidVec =
    WireInit(VecInit.fill(retireNum)(WireInit(VecInit((0 until retireNum).map(j => validPDestVec(j))))))
  selVec(0) := PriorityEncoder(validPDestVec)
  (1 until retireNum).map(i => {
    val temp = WireInit(VecInit((0 until retireNum).map(j => tempValidVec(i - 1)(j))))
    temp(selVec(i - 1)) := false.B
    tempValidVec(i)     := (tempValidVec(i - 1).asUInt & temp.asUInt).asBools
    selVec(i)           := PriorityEncoder(tempValidVec(i))
  })
  //valid and bits
  (0 until retireNum).map { i =>
    io.out.flRecover(i).valid := i.U < pushFlNum
    io.out.flRecover(i).bits  := DontCare
    when(io.out.flRecover(i).valid) {
      io.out.flRecover(i).bits := flrQueue(dontTouch(flrTailPtr + selVec(i)))
    }
  }
  //flrstate: recover/normal
  //REG for better frequency
  val dperRdyReg = RegInit(false.B)
  when(flrState === recover) {
    assert(!robEntries.io.flush)

    val nextFlrTailPtr = Mux(remainNum < retireNum.U, flrTailPtr + remainNum, flrTailPtr + retireNum.U)
    flrTailPtr := nextFlrTailPtr
    dperRdyReg := remainNum <= (7 * retireNum).U
    (0 until dispatchNum).foreach(i => { io.in.fromDispatcher(i).ready := dperRdyReg })

    when(flrHeadPtr === nextFlrTailPtr) {
      flrState   := idle
      flrTailPtr := 0.U
      flrHeadPtr := retireNum.U
      (0 until retireNum).map(i =>
        flrQueue(i) := Mux(mRe(i).valid & state =/= exEr, retireReg(i).uOp.prevPDest, 0.U(pRegAddrWidth.W))
      )
    }.otherwise {
      assert(!multiReVReg.asUInt.orR)
    }
  }.otherwise {
    //normal state
    flrTailPtr := 0.U
    flrHeadPtr := retireNum.U
    (0 until retireNum).map(i =>
      flrQueue(i) := Mux(mRe(i).valid & state =/= exEr, retireReg(i).uOp.prevPDest, 0.U(pRegAddrWidth.W))
    )

    /**
      * handle situation in preExEr and preNext
      *   wrong road inst will be poped(certainly not retire them)if its leading done==true
      *   when flush,these inst'Pdest can't get in flQueue,so need to recover them in these 2 states
      *   wrong inst include:
      *     preExer:exerAndBehind
      *     preNext:
      *       ci:behind ci
      *       mis:if ds exer dsAndBehind,else behindDs
      */
    val doneVec = WireInit(VecInit((0 until retireNum).map(i => retireReg(i).done)))
    val misIdx  = WireInit(2.U)
    when(firIdxReg + 1.U =/= retireNum.U) {
      val ds = retireReg(firIdxReg + 1.U)
      when(ds.done && (ds.exception.detect.happen | ds.uOp.specialType === SpecialType.ERET)) {
        misIdx := 1.U
      }
    }
    val idx =
      Mux(state === preExEr, 0.U, Mux(retireReg(firIdxReg).isMispredict, misIdx, 1.U))
    when(state === preExEr | state === preNext) {
      (0 until retireNum).map(i => {
        when(i.U >= firIdxReg && (i.U - firIdxReg) >= idx) {
          val mask = !doneVec.asUInt(i, 0).andR
          flrQueue(i) := Mux(mask, 0.U(pRegAddrWidth.W), retireReg(i).uOp.currPDest)
        }
      })
    }

    /**
      * not poped ds is in rob when misflush
      *   we can't let it get into flrQ(it should only push its prevPdest)
      *   note that ->dsRetire->misflush only when ds not exEr
      *     otherwise,it will ->exer->exerflush(get into flrQ and push its curPdest)
      */
    when(robEntries.io.flush) {
      assert(!multiReVReg.asUInt.orR)
      val realRobTail = WireInit(robEntries.tailIdx)
      dperRdyReg := (robEntries.headIdx - realRobTail) <= (6 * retireNum).U
      flrTailPtr := realRobTail
      flrHeadPtr := robEntries.headIdx
      when(state === misFlush & !dsPopReg) { realRobTail := robEntries.tailIdx + 1.U }
      (0 until robNum).foreach(i => { flrQueue(i) := robEntries.allPDest(i) })
      flrState := recover
    }
  }

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
