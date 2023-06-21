package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import frontend._
import utils.MultiQueue
import utils.asg

//0619:meet 3 debug...

class SimpleWriteBundle extends MycpuBundle {
  val wen   = Output(Bool())
  val wdata = Output(UWord)
}

class Mtc0Bundle extends SimpleWriteBundle {
  val addr = Output(CP0Idx)
}

class RobEntry extends MycpuBundle {
  //val pc          = UWord // difftest check execution flow
  //val prevPDest   = PRegIdx // free when retire
  //val currPDest   = PRegIdx // updata A-RAT when retire
  //val currADest   = ARegIdx // updata A-RAT when retire
  //val specialType = SpecialType()
  //val c0Addr      = CP0Idx //for mtc0,other dontcare
  val fromDispatcher = new DispatchToRobBundle
  val takeWord       = UWord //for ldst it's memReqVaddr,for mtxx it's wdata
  val exception      = new ExceptionInfoBundle
  val isMispredict   = Bool()
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
  *           1st mispredict and not be abandoned by prev exception
  *           its delayslot slotValid
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
          val scommit  = Output(Bool()) //to storeQ
          val toArchHi = new SimpleWriteBundle
          val toArchLo = new SimpleWriteBundle
          val eret     = Output(Bool()) //to CP0
          val mtc0     = new Mtc0Bundle //to CP0
        })
      )
      //to CP0
      val exception = new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      }
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
    enqData.exception    := DontCare
    enqData.isMispredict := DontCare
    enqData.takeWord     := DontCare
    asg(io.in.fromDispatcher(i).ready, robEnq(i).ready)
  })

  //WB
  val wdata = (0 until wBNum).map(i => io.in.wbRob(i).bits)
  // 报错1:似乎是寄存器就会报错？FIXME:
  List.tabulate(wBNum)(i =>
    when(io.in.wbRob(i).valid) {
      asg(robEntries.ringBuffer(wdata(i).robIndex).exception, wdata(i).exception)
      asg(robEntries.ringBuffer(wdata(i).robIndex).isMispredict, wdata(i).isMispredict)
      asg(robEntries.ringBuffer(wdata(i).robIndex).takeWord, wdata(i).takeWord)
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
    *           so we delay the "mispreFlushBackend" for a cycle
    *           be aware of delayslot exception...
    */

  //报错3:可以不解决(这一句会报错说：ready没有被init)
  //val retireSlot = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i))))
  val retireInst = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i).bits)))

  //exception|mispredict   be aware of retireSlot.Valid
  val mispredictVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).isMispredict && robEntries.io.pop(i).valid))
  )
  val exceptionVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).exception.happen && robEntries.io.pop(i).valid))
  )

  //mask from slot(1stMispredict+2) to the end:
  val mispredictMask = WireInit(VecInit(Seq.fill(retireNum)(false.B)))
  if (retireNum > 2) {
    (2 until retireNum).map(i => {
      asg(mispredictMask(i), mispredictVec(i - 2) | mispredictMask(i - 1))
    })
  }

  //disable mask:note that we do not abandon the 1st mispredict inst and its delayslot
  val abnormalVec = WireInit(VecInit((0 until retireNum).map(i => exceptionVec(i) | mispredictMask(i))))
  val disableMask = Wire(Vec(retireNum, Bool()))
  (0 until retireNum).map(i => {
    if (i == 0) { asg(disableMask(i), abnormalVec(i)) }
    else { asg(disableMask(i), abnormalVec(i) | disableMask(i - 1)) }
  })
  //disabled inst can't pop from rob
  List.tabulate(retireNum)(i => { asg(robEntries.io.pop(i).ready, !disableMask(i)) })

  /**
    * deal with retire.valid(retire端口)
    *   指令有效 & 非mispre退休的下一拍 &没被disableMask
    *   first Mispredict&notAbandon inst can't go until its delaySlot valid
    */
  val mispreFlushReg = RegInit(false.B) //mispre正常退休的下一拍将会被拉起
  val dSlotExceptReg = RegInit(false.B) //记录的是 被允许正常退休的mispre 的delaySlot是否异常
  (0 until retireNum).map(i => {
    asg(io.out.retire(i).valid, robEntries.io.pop(i).valid && !disableMask(i) && !mispreFlushReg)
  })
  //especially consider ***1st*** mispredict inst
  //note that the 2nd(or 3rd 4th...)mispreInst's abandon bits in disableMask is certainly true
  val lastIdx = retireNum - 1
  (0 until lastIdx).map(i => {
    when(robEntries.io.pop(i).valid && mispredictVec(i) && !disableMask(i) && !mispreFlushReg) {
      asg(io.out.retire(i).valid, robEntries.io.pop(i + 1).valid) //be aware:we use delayslot's retireSlot Valid
      asg(dSlotExceptReg, robEntries.io.pop(i + 1).valid & robEntries.io.pop(i + 1).bits.exception.happen)
    }
  })
  when(robEntries.io.pop(lastIdx).valid && !disableMask(lastIdx) && mispredictVec(lastIdx)) {
    asg(io.out.retire(lastIdx).valid, false.B)
  }

  /**
    * mispre flushBackend:
    *     延迟一拍才flushBackend，因为mispre也会修改arch状态
    *     在延迟的那一拍，所有slot都unvalid！(包括例外也不能提交)
    *     delaySlot是个特殊点，如果它发生了例外，那么在延迟的那一拍他还在，那么我们需要报出这个例外
    *       如果不延迟一拍，当拍mispre的话，下一拍ROB就空了，没走的例外延迟槽就em(不过其实这里搞个寄存位啥的也能处理)
    */
  val mispreCanRetire = WireInit( //notice we use out.retire(i).valid
    VecInit((0 until retireNum).map(i => io.out.retire(i).valid && mispredictVec(i)))
  ).asUInt.orR
  when(mispreCanRetire) { asg(mispreFlushReg, true.B) }
  when(io.out.mispreFlushBackend) {
    asg(mispreFlushReg, false.B)
    asg(dSlotExceptReg, false.B)
  }
  asg(io.out.mispreFlushBackend, mispreFlushReg)

  /**
    * exception
    *   - 在mispre退休延迟的那一拍不能报例外，除非oldest是上一拍没走掉的例外延迟槽
    *   - 我们只用oldest连接，因为同一拍，例外和正常指令不能同时退休
    */
  val oldestInst = retireInst(0)
  val oldestType = oldestInst.fromDispatcher.specialType
  asg(
    io.out.exception.badVaddr,
    Mux(
      (oldestType === SpecialType.LOAD || oldestType === SpecialType.STORE),
      oldestInst.takeWord, //for ldst,it's memReqVaddr
      oldestInst.fromDispatcher.pc
    )
  )
  io.out.exception.basic := DontCare //default
  asg(io.out.exception.basic.happen, false.B) //default=false
  when(robEntries.io.pop(0).valid && !(mispreFlushReg && !dSlotExceptReg)) {
    asg(io.out.exception.basic, oldestInst.exception)
  }

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
    asg(retireOut.toArchHi.wen, instType === SpecialType.MTHI)
    asg(retireOut.toArchLo.wen, instType === SpecialType.MTLO)
    asg(retireOut.mtc0.wen, instType === SpecialType.MTC0)
    //feed data/addr
    asg(retireOut.mtc0.addr, retireInst(i).fromDispatcher.c0Addr)
    List(retireOut.mtc0.wdata, retireOut.toArchHi.wdata, retireOut.toArchLo.wdata).map {
      case x =>
        asg(x, retireInst(i).takeWord)
    }
  })

  //exceptionVec already consider slotsValid
  //io.out.mispreFlushBackend is one cycle after mispre retire
  asg(robEntries.io.flush, io.out.mispreFlushBackend || exceptionVec(0))
}
