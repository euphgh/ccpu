package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import frontend._
import utils.MultiQueue
import utils.asg
import chisel3.util.experimental.BoringUtils

//0619:meet 3 debug...

class SingleRetireBundle extends MycpuBundle {
  val muldiv = Output(Bool())
  val mtlo   = Output(Bool())
  val mthi   = Output(Bool())
  val mtc0   = Output(Bool()) //to CP0
}

class RobEntry extends MycpuBundle {
  val fromDispatcher = new DispatchToRobBundle
  val exception      = new ExceptionInfoBundle
  val isMispredict   = Bool()
  val done           = Bool()
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
    }
    val out = new Bundle {
      val robIndex = Output(ROBIdx) //to dper
      val robEmpty = Output(Bool()) //to dper
      //valid = normal & instValid
      val multiRetire = Vec(
        retireNum,
        Valid(new Bundle {
          val toArat  = new RATWriteBackIO //to arat
          val scommit = Output(Bool()) //to storeQ
        })
      )
      //single Retire inst
      val singleRetire = Valid(new SingleRetireBundle)
      //to CP0
      val eretFlush = Output(Bool()) //to CP0
      val exception = Valid(new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      })
      //mispredict only FlushBackend
      val mispreFlushBackend = Output(Bool())
      val flushAll           = Output(Bool()) //serve as recover rat and hilo
      val ciRedirect         = Output(new FrontRedirctIO) //serve as recover rat and hilo
      //for uncache load inst
      val oldestIdx = Output(ROBIdx)
      // FreeList recover Ports
      val flRecover = Vec(retireNum, Valid(PRegIdx))
    }
  })
  class ROBQueue extends MultiQueue(dispatchNum, retireNum, new RobEntry, robNum) {
    val wb = IO(
      Vec(
        wBNum,
        new Bundle {
          val wen        = Input(Bool())
          val idx        = Input(UInt())
          val exception  = Input(new ExceptionInfoBundle)
          val misPredict = Input(Bool())
        }
      )
    )
    val headIdx  = IO(Output(UInt(robIndexWidth.W)))
    val tailIdx  = IO(Output(UInt(robIndexWidth.W)))
    val isEmpty  = IO(Output(Bool()))
    val allPDest = IO(Vec(robNum, Output(PRegIdx)))
    isEmpty := empty
    headIdx := headPtr
    tailIdx := tailPtr
    (0 until wBNum).foreach { i =>
      {
        when(wb(i).wen) {
          ringBuffer(wb(i).idx).done         := true.B
          ringBuffer(wb(i).idx).exception    := wb(i).exception
          ringBuffer(wb(i).idx).isMispredict := wb(i).misPredict
        }
      }
    }
    (0 until robNum).foreach(i => {
      allPDest(i) := ringBuffer(i).fromDispatcher.currPDest
    })
  }
  val robEntries = Module(new ROBQueue)
  io.out.oldestIdx := robEntries.io.tailPtr

  //RobEnqueue
  //Dontcare means write in WB stage
  val robEnq = robEntries.io.push
  List.tabulate(robEnq.length)(i => {
    val enqData = robEnq(i).bits
    asg(robEnq(i).valid, io.in.fromDispatcher(i).valid)
    asg(enqData.fromDispatcher, io.in.fromDispatcher(i).bits)
    asg(robEnq(i).bits.done, false.B)
    enqData.exception    := 0.U.asTypeOf(new ExceptionInfoBundle)
    enqData.isMispredict := false.B
    asg(io.in.fromDispatcher(i).ready, robEnq(i).ready)
  })

  //WB
  val wdata = (0 until wBNum).map(i => io.in.wbRob(i).bits)
  List.tabulate(wBNum)(i => {
    robEntries.wb(i).wen        := io.in.wbRob(i).valid
    robEntries.wb(i).idx        := wdata(i).robIndex
    robEntries.wb(i).exception  := wdata(i).exception
    robEntries.wb(i).misPredict := wdata(i).isMispredict
  })

  io.out.robEmpty := robEntries.isEmpty
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

  //报错3:可以不解决(这一句会报错说：ready没有被init)
  val retireInst   = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))
  val readyRetire  = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && retireInst(i).done)))
  val retireSpType = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).fromDispatcher.specialType)))
  //exception|mispredict   be aware of readyRetire
  val waitNextVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).isMispredict || retireSpType(i) === SpecialType.CACHEINST) && readyRetire(i)
      )
    )
  )
  val exerVec = WireInit(
    VecInit(
      (0 until retireNum).map(i =>
        (retireInst(i).exception.happen || retireSpType(i) === SpecialType.ERET) &&
          readyRetire(i)
      )
    )
  )
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
  val (firExEr, firMispredict, firSingle) = (
    WireDefault(retireNum.U(log2Up(retireNum + 1).W)),
    WireDefault(retireNum.U(log2Up(retireNum + 1).W)),
    WireDefault(retireNum.U(log2Up(retireNum + 1).W))
  )
  val vecList = List(exerVec, waitNextVec, singleRetireVec)
  val firList = List(firExEr, firMispredict, firSingle)
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
  val retireRdy = VecInit.fill(retireNum)(false.B)
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

  // init
  io.out.eretFlush          := false.B
  io.out.exception.valid    := false.B
  io.out.mispreFlushBackend := false.B
  io.out.flushAll           := false.B
  io.out.ciRedirect.flush   := false.B
  switch(state) {
    is(normal) {
      when(hasExer && firExEr < firMispredict && firExEr <= firSingle) {
        asg(state, exerFlush) //otherwise, we should delay the exception
        asg(retireRdy, VecInit(exerMask.asBools))
      }.elsewhen(hasWaitNext && firMispredict < firSingle) {
        asg(state, waitNext)
        asg(retireRdy, VecInit(waitNextMask.asBools))
      }
    }
    is(waitNext) {
      (0 until retireNum).map(i => asg(retireRdy(i), false.B)) //default
      when(readyRetire(0)) {
        when(exerVec(0) || retireSpType(0) === SpecialType.CACHEINST) {
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
    }
    is(exerFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => retireRdy(i) := false.B)
      io.out.flushAll := true.B

      when(retireInst(0).exception.happen) {
        io.out.exception.valid := true.B
      }.elsewhen(retireSpType(0) === SpecialType.CACHEINST) {
        io.out.eretFlush := true.B
      }.otherwise {
        io.out.ciRedirect.flush := true.B
      }
    }
  }
  asg(robEntries.io.flush, io.out.mispreFlushBackend || io.out.exception.valid || io.out.eretFlush)
  asg(io.out.exception.bits.basic, retireInst(0).exception)
  asg(io.out.ciRedirect.target, retireInst(0).fromDispatcher.pc)

  //exception connect
  val oldestInst   = retireInst(0)
  val oldestType   = oldestInst.fromDispatcher.specialType
  val memReqVaddr  = Wire(UWord)
  val memException = Wire(Bool())
  //TODO:addsource
  // BoringUtils.addSink(memReqVaddr, "badMemVaddrReg")
  // BoringUtils.addSink(memException, "MemExceptionReg") //无法通过exccode区分开load的取指/访存例外
  memReqVaddr  := 0.U
  memException := 0.U
  asg(
    io.out.exception.bits.badVaddr,
    Mux(
      (oldestType === SpecialType.LOAD || oldestType === SpecialType.STORE) && memException,
      memReqVaddr,
      oldestInst.fromDispatcher.pc
    )
  )
  asg(io.out.exception.bits.basic, oldestInst.exception)

  //multiRetire connect
  List.tabulate(retireNum)(i => {
    val retireOut = io.out.multiRetire(i).bits
    // asg(io.out.flRecover(i), retireInst(i).fromDispatcher.prevPDest)
    asg(retireOut.toArat.aDest, retireInst(i).fromDispatcher.currADest)
    asg(retireOut.toArat.pDest, retireInst(i).fromDispatcher.currPDest)
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
      io.out.flRecover(i).bits  := retireInst(i).fromDispatcher.prevPDest
    }
    // ROB push ready ===============================================================
    (0 until dispatchNum).foreach(i => {
      io.in.fromDispatcher(i).ready := robEntries.io.push(i).ready
    })
  }
}
