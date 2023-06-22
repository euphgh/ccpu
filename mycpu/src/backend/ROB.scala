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

class SimpleWriteBundle extends MycpuBundle {
  val wen   = Output(Bool())
  val wdata = Output(UWord)
}

class Mtc0Bundle extends SimpleWriteBundle {
  val addr = Output(CP0Idx)
}

class RobEntry extends MycpuBundle {
  val fromDispatcher = new DispatchToRobBundle
  val exception      = new ExceptionInfoBundle
  val isMispredict   = Output(Bool())
  val done           = Output(Bool())
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
          val prevDestPregAddr = Output(PRegIdx) //to fl
          val toArat           = new RATWriteBackIO //to arat
          val scommit          = Output(Bool()) //to storeQ
        })
      )
      //single Retire inst
      val singleRetire = Valid(new Bundle {
        val muldivCommit = Output(Bool())
        val mthiloCommit = Output(Bool())
        val mtc0Commit   = Output(Bool()) //to CP0
      })
      //to CP0
      val eretFlush = Output(Bool()) //to CP0
      val exception = Valid(new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      })
      //mispredict only FlushBackend
      val mispreFlushBackend = Output(Bool())
    }
  })
  val robEntries = Module(new MultiQueue(dispatchNum, retireNum, new RobEntry, robNum))

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
  // 报错1:似乎是寄存器就会报错？FIXME:
  // List.tabulate(wBNum)(i =>
  //   when(io.in.wbRob(i).valid) {
  //     asg(robEntries.ringBuffer(wdata(i).robIndex).done, true.B)
  //     asg(robEntries.ringBuffer(wdata(i).robIndex).exception, wdata(i).exception)
  //     asg(robEntries.ringBuffer(wdata(i).robIndex).isMispredict, wdata(i).isMispredict)
  //   }
  // )

  //out to dper
  //报错2:似乎是寄存器就会报错？FIXME:
  // asg(io.out.robEmpty, robEntries.empty)
  // asg(io.out.robIndex, robEntries.headPtr(robIndexWidth, 0))

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
  //val retireSlot = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i))))
  val retireInst   = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))
  val readyRetire  = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && retireInst(i).done)))
  val retireSpType = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).fromDispatcher.specialType)))
  //exception|mispredict   be aware of readyRetire
  val mispredictVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).isMispredict && readyRetire(i)))
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
          retireSpType(i) === SpecialType.MTHILO ||
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
  val vecList = List(exerVec, mispredictVec, singleRetireVec)
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

  List.tabulate(retireNum)(i => { asg(io.out.multiRetire(i).valid, true.B) }) //default
  List.tabulate(retireNum)(i => { asg(robEntries.io.pop(i).ready, io.out.multiRetire(i).valid) }) //default
  asg(io.out.singleRetire.valid, false.B) //default

  //automachine
  object RetireState extends ChiselEnum {
    val normal, waitDs, misFlush, exerFlush = Value
  }
  import RetireState._
  val hasExer       = exerVec.asUInt.orR
  val hasMispredict = mispredictVec.asUInt.orR
  val hasSingle     = singleRetireVec.asUInt.orR
  val state         = RegInit(normal)
  switch(state) {
    is(normal) {
      when(hasExer && firExEr < firMispredict && firExEr <= firSingle) {
        when(exerVec(0)) {
          //if first slot is exception,just flush and state not change
          (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, false.B))
        }.otherwise {
          asg(state, exerFlush) //otherwise, we should delay the exception
          (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, i.U < firExEr)) //retire normal inst
        }
      }.elsewhen(hasMispredict && firMispredict < firSingle) {
        asg(state, waitDs)
        (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, i.U <= firMispredict)) //don't let ds go
      }.elsewhen(hasSingle) {
        (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, i.U < firSingle))
        asg(robEntries.io.pop(firSingle).ready, true.B)
        asg(io.out.singleRetire.valid, true.B)
      }.otherwise {
        (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, true.B))
      }
    }
    is(waitDs) {
      (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, false.B)) //default
      when(readyRetire(0)) {
        when(exerVec(0)) {
          asg(state, normal) //next cycle the state should back to normal
        }.otherwise {
          asg(state, misFlush) //next cycle will mispreflushbackend
          asg(io.out.multiRetire(0).valid, true.B) //should let ds go
        }
      }
    }
    is(misFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, false.B))
    }
    is(exerFlush) {
      asg(state, normal)
      (0 until retireNum).map(i => asg(io.out.multiRetire(i).valid, false.B))
    }
  }

  asg(io.out.mispreFlushBackend, state === misFlush)
  asg(io.out.exception.valid, exerVec(0) && retireInst(0).exception.happen)
  asg(io.out.eretFlush, exerVec(0) && !io.out.exception.valid)
  asg(robEntries.io.flush, io.out.mispreFlushBackend || io.out.exception.valid || io.out.eretFlush)

  //exception connect
  val oldestInst   = retireInst(0)
  val oldestType   = oldestInst.fromDispatcher.specialType
  val memReqVaddr  = Wire(UWord)
  val memException = Wire(Bool())
  //TODO:addsource
  BoringUtils.addSink(memReqVaddr, "badMemVaddrReg")
  BoringUtils.addSink(memException, "MemExceptionReg") //无法通过exccode区分开load的取指/访存例外
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
    asg(retireOut.prevDestPregAddr, retireInst(i).fromDispatcher.prevPDest)
    asg(retireOut.toArat.aDest, retireInst(i).fromDispatcher.currADest)
    asg(retireOut.toArat.pDest, retireInst(i).fromDispatcher.currPDest)
    asg(retireOut.scommit, retireSpType(i) === SpecialType.STORE)
  })

  //singleRetire connect
  val sRetireList = List(
    io.out.singleRetire.bits.mtc0Commit,
    io.out.singleRetire.bits.mthiloCommit,
    io.out.singleRetire.bits.muldivCommit
  )
  (0 until sRetireList.length).map(i => asg(sRetireList(i), false.B)) //default
  val spList = List(SpecialType.MTC0, SpecialType.MTHILO, SpecialType.MULDIV)
  when(io.out.singleRetire.valid) {
    List.tabulate(sRetireList.length)(i => {
      asg(sRetireList(i), retireSpType(firSingle) === spList(i))
    })
  }
}
