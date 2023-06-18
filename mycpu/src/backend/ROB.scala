package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import frontend._
import utils.MultiQueue
import utils.asg

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
  *     1.normal(not exception and mispredict)
  *       just use <prevDestPregAddr> to push freelist
  *       change the a-rat:
  *            use destPregAddr/destAregAddr
  *
  *     2.special
  *       toCP0
  *         exception<abnormal>
  *           <exception>:as basic to CP0
  *           <memReqVaddr>:as badVaddr to CP0
  *          mtc0
  *          eret
  *       mispredict<abnormal>
  *         flushBackend(will unlock renameStage.valid)
  *       storeInst
  *         send a scommit to storeQueue
  *       hilo
  *         send a wen to arch-hilo
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
      val normalRetire = Vec(
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
      //mispredict only flushBackend
      val flushBackend = Output(Bool())
    }
  })
  val robSize    = 32
  val robEntries = Module(new MultiQueue(dispatchNum, retireNum, new RobEntry, robSize))

  //RobEnqueue
  //Dontcare means write in WB stage
  val robEnq = robEntries.io.push
  List.tabulate(robEnq.length)(i => {
    val enqData = robEnq(i).bits
    asg(robEnq(i).valid, io.in.fromDispatcher(i).valid)
    asg(enqData.fromDispatcher, io.in.fromDispatcher(i).bits)
    asg(enqData.exception, DontCare)
    asg(enqData.isMispredict, DontCare)
    asg(enqData.takeWord, DontCare)
    asg(io.in.fromDispatcher(i).ready, robEnq(i).ready)
  })

  //WB
  List.tabulate(wBNum)(i =>
    when(io.in.wbRob(i).valid) {
      val wdata  = io.in.wbRob(i).bits
      val wbSlot = robEntries.ringBuffer(wdata.robIndex)
      asg(wbSlot.exception, wdata.exception)
      asg(wbSlot.isMispredict, wdata.isMispredict)
      asg(wbSlot.takeWord, wdata.takeWord)
    }
  )

  //out to dper
  asg(io.out.robEmpty, robEntries.empty)
  asg(io.out.robIndex, robEntries.headPtr)

  /**
    * retire:
    *     this version is kind of trouble:
    *       normal and abnormal can retire at one cycle
    *       but consider of arat-srat(ahilo-shilo),it's a little complicated
    *       so need another version
    */
  List.tabulate(retireNum)(i => asg(retireSlot(i).ready, true.B))
  val retireSlot = WireInit(VecInit((0 until retireNum).map(i => robEntries.io.pop(i))))
  val retireInst = WireInit(VecInit((0 until retireNum).map(i => retireSlot(i).bits)))

  //exception/mispredict
  //be aware of retireSlot.Valid
  val exceptionVec = WireInit(
    VecInit((0 until retireNum).map(i => retireInst(i).exception.happen && retireSlot(i).valid))
  )
  val mispredictVec = WireInit(VecInit((0 until retireNum).map(i => retireInst(i).isMispredict && retireSlot(i).valid)))
  val abnormalVec   = WireInit(VecInit((0 until retireNum).map(i => exceptionVec(i) | mispredictVec(i))))
  val abandonMask   = Wire(Vec(retireNum, Bool()))
  (0 until retireNum).map(i => {
    if (i == 0) { asg(abandonMask(i), abnormalVec(i)) }
    else { asg(abandonMask(i), abnormalVec(i) | abandonMask(i - 1)) }
  })

  /**
    * normal retire:
    *   valid = instValid & !abnormalMask
    *   feed retire data
    */
  List.tabulate(retireNum)(i => {
    val retireOut = io.out.normalRetire(i).bits
    //basic
    asg(io.out.normalRetire(i).valid, retireSlot(i).valid && !abandonMask(i))
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

  /**
    * abnormal retire:
    *   mispredict or exception.happen
    */
  asg(io.out.exception.badVaddr, DontCare) //default,change when exception happen before any mispredict
  asg(io.out.exception.basic, DontCare) //default
  asg(io.out.exception.basic.happen, false.B) //default = false
  asg(io.out.flushBackend, false.B) //default = false
  //sel the first abnormal retireInst if abnormal
  val abnormal    = abnormalVec.asUInt.orR
  val abnormalSel = UInt(log2Up(retireNum).W)
  when(abnormal) {
    asg(
      abnormalSel,
      PriorityMux(
        (0 until retireNum).map(i => abnormalVec(i)),
        (0 until retireNum).map(_.U)
      )
    )
    val abInst     = retireInst(abnormalSel)
    val abInstType = abInst.fromDispatcher.specialType
    asg(io.out.exception.basic, abInst.exception) //happen or not is recorded in exception bundle
    asg(
      io.out.exception.badVaddr,
      Mux(
        (abInstType === SpecialType.LOAD || abInstType === SpecialType.STORE),
        abInst.takeWord, //for ldst,it's memReqVaddr
        abInst.fromDispatcher.pc
      )
    )
    //consider a inst mispredict and exception at the same time,we should ↑exception and ↑mispredict
    asg(io.out.flushBackend, mispredictVec(abnormalSel))
  }

}
