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
  *     1.Able
  *       normal one:
  *         just use <prevDestPregAddr> to push freelist
  *         change the a-rat:
  *            use destPregAddr/destAregAddr
  *       special inst:
  *           mtc0/eret/hilo/store
  *       mispredict:
  *           1st mispredict which not be abandoned by prev exception
  *           its delayslot slotValid(if !exception)
  *
  *     2.disable
  *         exception abandon itself and its little brother
  *         mispredict abandon inst after its delayslot
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
      val retire = Vec(
        retireNum,
        Valid(new Bundle {
          val prevDestPregAddr = Output(PRegIdx) //to fl
          val toArat           = new RATWriteBackIO //to arat
          //special inst
          val scommit      = Output(Bool()) //to storeQ
          val muldivCommit = Output(Bool())
          val mthiCommit   = Output(Bool())
          val mtloCommit   = Output(Bool())
          val mtc0Commit   = Output(Bool()) //to CP0
          val eret         = Output(Bool()) //to CP0
        })
      )
      //to CP0
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
  List.tabulate(wBNum)(i =>
    when(io.in.wbRob(i).valid) {
      asg(robEntries.ringBuffer(wdata(i).robIndex).done, true.B)
      asg(robEntries.ringBuffer(wdata(i).robIndex).exception, wdata(i).exception)
      asg(robEntries.ringBuffer(wdata(i).robIndex).isMispredict, wdata(i).isMispredict)
    }
  )

  //out to dper
  //报错2:似乎是寄存器就会报错？FIXME:
  asg(io.out.robEmpty, robEntries.empty)
  asg(io.out.robIndex, robEntries.headPtr(robIndexWidth, 0))

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
  val retireInst  = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))
  val readyRetire = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).valid && retireInst(i).done)))

  //exception|mispredict   be aware of readyRetire
  val mispredictVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).isMispredict && readyRetire(i)))
  )
  val exceptionVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).exception.happen && readyRetire(i)))
  )

  //sel the first mispredict and exception
  val firException  = retireNum.U //default
  val firMispredict = retireNum.U //default
  val hasException  = exceptionVec.asUInt.orR
  val hasMispredict = mispredictVec.asUInt.orR
  when(hasException) { //prevent undefine assign
    asg(
      firException,
      PriorityMux(
        (0 until retireNum).map(i => exceptionVec(i)),
        (0 until retireNum).map(_.U)
      )
    )
  }
  when(hasMispredict) { //prevent undefine assign
    asg(
      firMispredict,
      PriorityMux(
        (0 until retireNum).map(i => mispredictVec(i)),
        (0 until retireNum).map(_.U)
      )
    )
  }

  //automachine
  asg(io.out.exception.valid, false.B) //default
  asg(io.out.mispreFlushBackend, false.B) //default
  object RetireState extends ChiselEnum {
    val normal, waitDs, misFlush, exceptFlush = Value
  }
  import RetireState._
  val state = RegInit(normal)
  switch(state) {
    is(normal) {
      when(hasException && (!hasMispredict || firException < firMispredict)) {
        when(exceptionVec(0)) {
          asg(io.out.exception.valid, true.B) //if first slot is exception,just flush and state not change
          (0 until retireNum).map(i => asg(io.out.retire(i).valid, false.B))
        }.otherwise {
          asg(state, exceptFlush) //otherwise, we should delay the exception
          (0 until retireNum).map(i => asg(io.out.retire(i).valid, i.U < firException)) //retire normal inst
        }
      }.elsewhen(hasMispredict) {
        asg(state, waitDs)
        (0 until retireNum).map(i => asg(io.out.retire(i).valid, i.U <= firMispredict)) //don't let ds go
      }.otherwise {
        (0 until retireNum).map(i => asg(io.out.retire(i).valid, true.B))
      }
    }
    is(waitDs) {
      (0 until retireNum).map(i => asg(io.out.retire(i).valid, false.B)) //default
      when(readyRetire(0)) {
        when(exceptionVec(0)) {
          asg(state, normal) //next cycle the state should back to normal
          asg(io.out.exception.valid, true.B) //call an exception this cycle
        }.otherwise {
          asg(state, misFlush) //next cycle will mispreflushbackend
          asg(io.out.retire(0).valid, true.B) //should let ds go
        }
      }
    }
    is(misFlush) {
      asg(state, normal)
      asg(io.out.mispreFlushBackend, true.B)
      (0 until retireNum).map(i => asg(io.out.retire(i).valid, false.B))
    }
    is(exceptFlush) {
      asg(state, normal)
      asg(io.out.exception.valid, true.B)
      (0 until retireNum).map(i => asg(io.out.retire(i).valid, false.B))
    }
  }
  List.tabulate(retireNum)(i => { asg(robEntries.io.pop(i).ready, io.out.retire(i).valid) })
  asg(robEntries.io.flush, io.out.mispreFlushBackend || io.out.exception.valid)

  //exception connect
  val oldestInst   = retireInst(0)
  val oldestType   = oldestInst.fromDispatcher.specialType
  val memReqVaddr  = UWord
  val memException = Bool()
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

  //simple connect for <retire>
  List.tabulate(retireNum)(i => {
    val retireOut = io.out.retire(i).bits
    //basic
    asg(retireOut.prevDestPregAddr, retireInst(i).fromDispatcher.prevPDest)
    asg(retireOut.toArat.aDest, retireInst(i).fromDispatcher.currADest)
    asg(retireOut.toArat.pDest, retireInst(i).fromDispatcher.currPDest)
    //special inst
    val instType = retireInst(i).fromDispatcher.specialType
    asg(retireOut.eret, instType === SpecialType.ERET)
    asg(retireOut.scommit, instType === SpecialType.STORE)
    asg(retireOut.mthiCommit, instType === SpecialType.MTHI)
    asg(retireOut.mtloCommit, instType === SpecialType.MTLO)
    asg(retireOut.mtc0Commit, instType === SpecialType.MTC0)
  })
}
