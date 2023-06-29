package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import frontend._
import utils.MultiQueue
import utils.asg
import chisel3.util.experimental.BoringUtils._

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
  val done         = Bool()
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
      val wbRob         = Vec(wBNum, Flipped(Valid(new WbRobBundle)))
      val misPredictIdx = Input(ROBIdx)
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
        }
      )
    )
    val headIdx   = IO(Output(UInt(robIndexWidth.W)))
    val tailIdx   = IO(Output(UInt(robIndexWidth.W)))
    val isEmpty   = IO(Output(Bool()))
    val allPDest  = IO(Vec(robNum, Output(PRegIdx)))
    val mispreIdx = IO(Input(ROBIdx))
    val dsAllow   = IO(Output(Bool()))
    isEmpty := empty
    headIdx := headPtr
    tailIdx := tailPtr
    (0 until wBNum).foreach { i =>
      {
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
    dsAllow := (ds.done || ds.uOp.specialType =/= SpecialType.CACHEINST) && (headIdx =/= mispreIdx + 1.U)
  }
  val robEntries = Module(new ROBQueue)
  io.out.oldestIdx     := robEntries.io.tailPtr
  robEntries.mispreIdx := io.in.misPredictIdx
  io.out.dsAllow       := robEntries.dsAllow

  //RobEnqueue
  //Dontcare means write in WB stage
  val robEnq = robEntries.io.push
  List.tabulate(robEnq.length)(i => {
    val enqData  = robEnq(i).bits
    val fromDper = io.in.fromDispatcher(i).bits
    asg(enqData.uOp, fromDper.uOp)
    asg(enqData.exception.basic, fromDper.basicExInfo)
    asg(enqData.done, false.B)
    asg(enqData.isMispredict, false.B)
    asg(enqData.exception.detect, 0.U.asTypeOf(new DetectExInfoBundle))
    asg(robEnq(i).valid, io.in.fromDispatcher(i).valid)
    asg(io.in.fromDispatcher(i).ready, robEnq(i).ready)
  })

  //WB
  val wdata = (0 until wBNum).map(i => io.in.wbRob(i).bits)
  List.tabulate(wBNum)(i => {
    robEntries.wb(i).wen        := io.in.wbRob(i).valid
    robEntries.wb(i).idx        := wdata(i).robIndex
    robEntries.wb(i).exDetect   := wdata(i).exDetect
    robEntries.wb(i).misPredict := wdata(i).isMispredict
  })

  //io.out.robEmpty := robEntries.isEmpty
  asg(io.out.robIndex, robEntries.headIdx)

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
  val readyRetire  = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && retireInst(i).done)))
  val retireSpType = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).uOp.specialType)))
  //waitNext--msipredict | cacheinst
  val waitNextVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).isMispredict || retireSpType(i) === SpecialType.CACHEINST) && readyRetire(i)
      )
    )
  )
  //exception | eret
  val exerVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).exception.detect.happen || retireSpType(i) === SpecialType.ERET) &&
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
  val retireRdy = VecInit.fill(retireNum)(false.B) //default
  (0 until retireNum).foreach(i => { robEntries.io.pop(i).ready := readyRetire(i) })
  asg(io.out.singleRetire.valid, false.B) //default

  //automachine
  object RetireState extends ChiselEnum {
    val normal, waitNext, misFlush, exerFlush = Value
  }
  import RetireState._
  val hasExer     = exerVec.asUInt.orR
  val hasWaitNext = waitNextVec.asUInt.orR
  val hasSingle   = singleRetireVec.asUInt.orR
  val state       = RegInit(normal)
  import utils._
  val normalSel    = PriorityVec(VecInit(exerVec.asUInt | singleRetireVec.asUInt, waitNextVec.asUInt))
  val exerMask     = PriorityMask(exerVec.asUInt)
  val waitNextMask = Cat(PriorityMask(waitNextVec.asUInt)(retireNum - 2, 0), 0.U(1.W))
  // JMP HB =======================================================
  val findHBinRob  = RegInit(false.B)
  val dstHBFromAlu = Flipped(Valid(UWord))
  addSink(dstHBFromAlu, "hbdest")
  val dstHB = Module(new Mark(UWord))
  dstHB.start <> dstHBFromAlu
  dstHB.end := io.out.flushAll

  // init
  io.out.eretFlush          := false.B
  io.out.exCommit.valid     := false.B
  io.out.mispreFlushBackend := false.B
  io.out.flushAll           := false.B
  io.out.robRedirect.flush  := false.B
  switch(state) {
    is(normal) {
      when(hasExer && firExEr <= firWaitNext && firExEr <= firSingle) {
        asg(state, exerFlush)
        asg(retireRdy, VecInit(exerMask.asBools)) //mask itself and the inst behind
      }.elsewhen(hasWaitNext && firWaitNext < firSingle) {
        asg(state, waitNext)
        asg(retireRdy, VecInit(waitNextMask.asBools)) //mask the inst behind
        asg(findHBinRob, retireSpType(firWaitNext) === SpecialType.HB)
      }
    }
    is(waitNext) { //CACHEINST 或 MISPRE(JRHB)转移而来
      (0 until retireNum).map(i => asg(retireRdy(i), false.B)) //default
      when(readyRetire(0)) {
        when(exerVec(0) || !retireInst(0).exception.basic.isBd) { //不是分支延迟槽(前一条是CI)或分支延迟槽异常
          asg(state, exerFlush)
        }.otherwise {
          asg(state, misFlush)
          retireRdy(0) := true.B
        }
      }
    }
    is(misFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => asg(retireRdy(i), false.B))
      io.out.mispreFlushBackend := true.B
      when(findHBinRob) {
        io.out.flushAll          := true.B
        io.out.robRedirect.flush := true.B
        findHBinRob              := false.B
      }
    }
    is(exerFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => retireRdy(i) := false.B)
      io.out.flushAll := true.B

      when(retireInst(0).exception.detect.happen) { //exception
        io.out.exCommit.valid := true.B
      }.elsewhen(retireSpType(0) === SpecialType.ERET) { //eret
        io.out.eretFlush := true.B
      }.otherwise {
        io.out.robRedirect.flush := true.B //cacheinst
      }
    }
  }
  val retirePcVal = retireInst(0).exception.basic.pc //pc记录在robEntry中
  asg(robEntries.io.flush, io.out.mispreFlushBackend || io.out.exCommit.valid || io.out.eretFlush)
  asg(io.out.robRedirect.target, Mux(state === exerFlush, retirePcVal, dstHB.value.bits)) //CI_NEXT或DSTHB

  //exception connect
  val oldestInst = retireInst(0)
  val oldestType = oldestInst.uOp.specialType
  val exCommit   = io.out.exCommit.bits
  // Mem badAddress =========================================
  val memReqVaddr     = Wire(UWord)
  val memException    = Wire(Bool())
  val badAddrFromMem1 = Flipped(Valid(UWord))
  addSink(badAddrFromMem1, "mem1BadAddr")
  val badAddr = Module(new Mark(UWord))
  badAddr.start <> badAddrFromMem1
  badAddr.end  := io.out.flushAll
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

  //multiRetire connect
  List.tabulate(retireNum)(i => {
    val retireOut = io.out.multiRetire(i).bits
    // asg(io.out.flRecover(i), retireInst(i).fromDispatcher.prevPDest)
    asg(retireOut.toArat.aDest, retireInst(i).uOp.currADest)
    asg(retireOut.toArat.pDest, retireInst(i).uOp.currPDest)
    asg(retireOut.scommit, retireSpType(i) === SpecialType.STORE)
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

  class FLRQueueEntry extends MycpuBundle {
    val pDestIdx = PRegIdx
    val valid    = Bool()
  }
  // pDest collect Queue
  val flrQueue   = Reg(Vec(robNum, PRegIdx))
  val flrHeadPtr = RegInit(0.U(log2Ceil(robNum).W))
  val flrTailPtr = RegInit(0.U(log2Ceil(robNum).W))
  object FreeListRecover extends ChiselEnum {
    val idle, recover = Value
  }
  import FreeListRecover._
  val flrState = RegInit(FreeListRecover.idle)
  when(robEntries.io.flush) {
    (0 until robNum).foreach(i => {
      flrQueue(i) := robEntries.allPDest(i)
      flrHeadPtr  := robEntries.headIdx
      flrTailPtr  := robEntries.tailIdx
    })
    flrState := recover
  }
  when(flrHeadPtr === flrTailPtr) { flrState := idle }
  when(flrState === recover) {
    val flrPopValid = VecInit((0 until retireNum).map(flrQueue(_) === 0.U)).asUInt
    flrTailPtr := PriorityEncoder(flrPopValid)
    // FreeList Push Valid ==========================================================
    (0 until retireNum).foreach { i =>
      val recoverValid = VecInit((0 to i).map(flrQueue(_) =/= 0.U)).asUInt.andR
      io.out.flRecover(i).valid :=
        (0 to i).map(i => flrQueue(i) =/= 0.U).foldLeft(0.U)(_ & _)
      io.out.flRecover(i).bits := flrQueue(i)
    }
    // ROB push ready ===============================================================
    (0 until dispatchNum).foreach(i => {
      io.in.fromDispatcher(i).ready := flrHeadPtr - flrTailPtr <= 8.U
    })
  }.otherwise {
    // FreeList Push Valid ==========================================================
    (0 until retireNum).foreach { i =>
      val recoverValid = VecInit((0 to i).map(flrQueue(_) =/= 0.U)).asUInt.andR
      io.out.flRecover(i).valid := io.out.multiRetire(i).valid
      io.out.flRecover(i).bits  := retireInst(i).uOp.prevPDest
    }
    // ROB push ready ===============================================================
    (0 until dispatchNum).foreach(i => {
      io.in.fromDispatcher(i).ready := robEntries.io.push(i).ready
    })
  }
}
