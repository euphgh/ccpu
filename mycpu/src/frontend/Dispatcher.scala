package frontend
import config._
import bundle._
import chisel3._
import chisel3.util._
import utils._
import chisel3.util.Cat
import chisel3.util.experimental.decode.QMCMinimizer
import chisel3.experimental.conversions._
import difftest.DifftestPhyRegInFreeList
import chisel3.util.experimental.BoringUtils._

class RATWriteBackIO extends MycpuBundle {
  val aDest = ARegIdx
  val pDest = PRegIdx
}
class MispreSignal extends MycpuBundle {
  val happen     = Output(Bool())
  val realTarget = Output(UWord)
  val robIdx     = Output(ROBIdx)
}

/**
  * WAW
  *  wb change inprf to true if(wb.p = srat(wb.a).p)
  *  dest change inprf to false,change pIdx(younger has higher priority)
  *
  * RAW
  *   (wb.a = someInst.a) & (wb.p = srat(wb.a).p) -> change in prf
  *   dest.a = younger.a  -> change inprf and pdest and prev(younger higher priority)
  *
  * about InstARegsIdxBundle:(src0, src1, dest)
  *      dest is 0 if !wen,else = destAreg
  *      src is 0 if !needSrcAreg,else = srcAregs
  *
  * wb.valid is pipex_valid
  * currPDest.valid is dper.slot fire
  */
class SRAT extends MycpuModule {
  val io = IO(new Bundle {
    val src = Vec(
      renameNum,
      new Bundle {
        val in  = Input(Vec(srcDataNum, ARegIdx)) //to get srcs p
        val out = Output(Vec(srcDataNum, new SRATEntry)) //srcs p
      }
    )
    val dest = Vec(
      renameNum,
      new Bundle {
        val currADest = Input(ARegIdx) //to get prev
        val prevPDest = Output(PRegIdx) //prev
        val currPDest = Flipped(Valid(PRegIdx)) //to write in
      }
    )
    val wb      = Vec(wBNum, Flipped(Valid(new RATWriteBackIO)))
    val recover = Flipped(Valid(Vec(aRegNum, new SRATEntry)))
  })

  //areg0 -> (0,true)
  val pIdxMap = RegInit(VecInit((0 until aRegNum).map(i => i.U(pRegAddrWidth.W))))
  val inPrf   = RegInit(VecInit(Seq.fill(aRegNum)(true.B)))

  //read from srat:lowest priority
  //num0 areg will get preg=0,inprf=1
  List.tabulate(renameNum)(i => {
    List.tabulate(srcDataNum)(j => {
      io.src(i).out(j).inPrf := inPrf(io.src(i).in(j))
      io.src(i).out(j).pIdx  := pIdxMap(io.src(i).in(j))
    })
    io.dest(i).prevPDest := pIdxMap(io.dest(i).currADest)
  })

  //wb change inprf:middle priority
  //io.wb.valid is pipex_valid(whether has valid inst)
  //aDest=0 actually not change anything
  List.tabulate(wBNum)(i =>
    when(io.wb(i).valid && (pIdxMap(io.wb(i).bits.aDest) === io.wb(i).bits.pDest)) {
      inPrf(io.wb(i).bits.aDest) := true.B
      List.tabulate(renameNum)(j => {
        List.tabulate(srcDataNum)(k =>
          when(io.wb(i).bits.aDest === io.src(j).in(k)) { io.src(j).out(k).inPrf := true.B }
        )
      })
    }
  )

  //dest change inprf:higest priority
  //io.dest.currPDest.valid = dper.slot.out.fire
  //if io.dest.currADest===0 ,means !wen
  //WAW(only last pDest write in)
  List.tabulate(renameNum)(i => {
    when(io.dest(i).currPDest.valid & io.dest(i).currADest =/= 0.U) {
      pIdxMap(io.dest(i).currADest) := io.dest(i).currPDest.bits
      inPrf(io.dest(i).currADest)   := false.B

      ((i + 1) until renameNum).map(j => {
        List.tabulate(srcDataNum)(k => {
          when(io.dest(i).currADest === io.src(j).in(k)) {
            io.src(j).out(k).inPrf := false.B
            io.src(j).out(k).pIdx  := io.dest(i).currPDest.bits
          }
          when(io.dest(i).currADest === io.dest(j).currADest) {
            io.dest(j).prevPDest := io.dest(i).currPDest.bits
          }
        })
      })
    }
  })
  when(io.recover.valid) {
    List.tabulate(aRegNum)(i => {
      asg(pIdxMap(i), io.recover.bits(i).pIdx)
      asg(inPrf(i), io.recover.bits(i).inPrf)
    })
  }

  def read(aRegsIdx: Vec[InstARegsIdxBundle]) = {
    require(aRegsIdx.length == renameNum)
    List.tabulate(renameNum)(i => {
      this.io.src(i).in(0)      := aRegsIdx(i).src0
      this.io.src(i).in(1)      := aRegsIdx(i).src1
      this.io.dest(i).currADest := aRegsIdx(i).dest
    })
    (0 until renameNum).map(i => (this.io.src(i).out, this.io.dest(i).prevPDest))
  }
}

//combination logic decode
class Decoder extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val instr     = Input(UWord)
      val exception = Input(FrontExcCode())
    }
    val out = new Bundle {
      val decoded   = new DecodeInstInfoBundle
      val exception = new DetectExInfoBundle
    }
  })

  //decode
  val instr   = io.in.instr
  val decoder = Wire(new DecodeInstInfoBundle)
  decoder.decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
  asg(io.out.decoded, decoder)

  /**
    * exception handle
    *   note that input is frontExcCode，output is detectExInfoBundle
    *   here we detect 系统调用/保留指令
    *   priority:前面的fetch/tlbl > 保留指令例外 > 系统调用/BREAK
    */
  val deExType  = decoder.decodeExcType
  val ri        = deExType === DeExType.RI //TODO:保留指令例外
  val syscall   = deExType === DeExType.SYS
  val break     = deExType === DeExType.BP
  val inExcCode = io.in.exception
  val inIsEx    = FrontExcCode.happen(inExcCode)
  val trap      = decoder.aluType.isOneOf(AluType.TRAPEQ, AluType.TRAPNE)
  val outEx     = io.out.exception
  //connect
  asg(outEx.happen, inIsEx || ri || syscall || break || trap)
  asg(outEx.refill, FrontExcCode.isRefill(inExcCode))
  asg(
    outEx.excCode,
    MuxCase(
      ExcCode.AdEL, //dontcare,no exception happen
      Seq(
        inIsEx  -> FrontExcCode.trans(inExcCode),
        ri      -> ExcCode.RI,
        syscall -> ExcCode.Sys,
        break   -> ExcCode.Bp,
        trap    -> ExcCode.Tr
      )
    )
  )
}

class dispatchSlot extends MycpuBundle {
  val inst  = new InstBufferOutIO
  val valid = Output(Bool())

  val pDestOk  = Output(Bool()) // not need pdest/related fl slot pop valid
  val robReady = Output(Bool()) //rob是否有相应的空闲槽位，此处无需设置“leadingRobReady”
  val rsReady  = Output(Bool()) //指令对应的rs是否"对其"ready(注意同类型指令只有第一条才会“得到”Rdy)

  val readyGo = Output(Bool())

  //  exceptDetect    decoder-Rs
  //  destPregAddr    freelist-Rs & Rob
  //  srcPregs        srat-Rs
  //  robIndex        Rs
  val toRsBasic = new RsBasicEntry
  val decoded   = new DecodeInstInfoBundle
  //srat-Rob
  val prevPDest = PRegIdx
}

/**
  * 1. instantiate MultiQueue as freeList in Dispatcher
  * 2. instantiate 3 Decoder
  * 3. instantiate SRAT, code solve WAW/RAW should write in it.
  *    SRAT should listen to write prf signal to
  *    update SRATEntry' inPrf
  *
  * Dispatcher may block for blow reason
  * 1. rob not enough
  * 2. freelist not enough
  * 3. rs conflict
  * 4. cache inst, cp0 read inst<block inst>
  * 5. mispredict
  */

class Dispatcher extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromInstBuffer = Vec(decodeNum, Flipped(Valid(new InstBufferOutIO)))
      val fuWbSrat       = Vec(wBNum, Flipped(Valid(new RATWriteBackIO)))
      val robIndex       = Input(ROBIdx)
      val flushBackend   = Input(Bool())
    }
    val outFireNum = Output(UInt((log2Up(dispatchNum) + 1).W))

    val fromAluMispre = new Bundle {
      val happen     = Input(Bool())
      val realTarget = Input(UWord)
    }
    val dsAllow      = Input(Bool())
    val fronRedirect = new FrontRedirctIO

    //valid when flush(mispredictRetire/exception/eret)
    // next cycle the backend is empty
    val recoverSrat = Flipped(Valid(Vec(aRegNum, new SRATEntry)))
    val pushFl      = Flipped(Vec(retireNum, Valid(PRegIdx)))

    val out = new Bundle {
      val toMainAluRs = Decoupled(new RsOutIO(kind = FuType.MainAlu))
      val toSubAluRs  = Decoupled(new RsOutIO(kind = FuType.SubAlu))
      val toMduRs     = Decoupled(new RsOutIO(kind = FuType.Mdu))
      val toLsuRs     = Decoupled(new RsOutIO(kind = FuType.Lsu))
      val toRob       = Vec(dispatchNum, Decoupled(new DispatchToRobBundle))
    }
  })

  //some inst can go to main/sub alurs;some can only goto sub aluRs
  val noInst = (dispatchNum).U
  def getRsSel(rsType: UInt): UInt = PriorityEncoderOH(
    (0 until dispatchNum).map(i => (slots(i).inst.whichFu === ChiselFuType(rsType) && slots(i).valid)).asUInt
  )
  def getMainALUSlot(): UInt = {
    val mainMask =
      (0 until dispatchNum).map(i => (slots(i).inst.whichFu === ChiselFuType.MainALU && slots(i).valid)).asUInt
    val subMask =
      (0 until dispatchNum).map(i => (slots(i).inst.whichFu === ChiselFuType.SubALU && slots(i).valid)).asUInt
    val hasMain = mainMask.orR
    Mux(hasMain, PriorityEncoderOH(mainMask), PriorityEncoderOH(subMask))
  }
  def getSubALUSlot(): UInt = {
    val mainMask =
      (0 until dispatchNum).map(i => (slots(i).inst.whichFu === ChiselFuType.MainALU && slots(i).valid)).asUInt
    val subMask =
      (0 until dispatchNum).map(i => (slots(i).inst.whichFu === ChiselFuType.SubALU && slots(i).valid)).asUInt
    val hasMain = mainMask.orR
    Mux(hasMain, PriorityEncoderOH(subMask), SecondPriEncoder(subMask))
  }

  class FreeList extends MultiQueue(retireNum, dispatchNum, PRegIdx, freeListSize, false, true) {
    if (verilator) {
      val difftestFreeList = Module(new DifftestPhyRegInFreeList)
      difftestFreeList.io.clock  := clock
      difftestFreeList.io.en     := true.B
      difftestFreeList.io.flHead := headPtr
      difftestFreeList.io.flTail := tailPtr
      difftestFreeList.io.fl     := ringBuffer
    }
  }

  val freeList = Module(new FreeList)
  asg(freeList.io.flush, false.B)
  (0 until retireNum).map(i => {
    asg(freeList.io.push(i).valid, io.pushFl(i).valid)
    asg(freeList.io.push(i).bits, io.pushFl(i).bits)
    when(io.pushFl(i).valid) {
      assert(io.pushFl(i).bits =/= 0.U)
    }
  })

  val decoder = List.fill(decodeNum)(Module(new Decoder()))
  val srat    = Module(new SRAT)
  val slots   = Wire(Vec(dispatchNum, new dispatchSlot))

  //just connect(combinational logic)
  //input:ibf/rob/fl
  List.tabulate(dispatchNum)(i => {
    slots(i).inst  := io.in.fromInstBuffer(i).bits
    slots(i).valid := io.in.fromInstBuffer(i).valid

    slots(i).toRsBasic.destAregAddr := slots(i).inst.aRegsIdx.dest
    slots(i).toRsBasic.destPregAddr := 0.U(pRegAddrWidth.W) //default
    slots(i).toRsBasic.robIndex     := io.in.robIndex + i.U
    slots(i).toRsBasic.prevPDest    := slots(i).prevPDest

    if (debug) slots(i).toRsBasic.debugPC.get := io.in.fromInstBuffer(i).bits.basicInstInfo.pcVal

    slots(i).robReady := io.out.toRob(i).ready
    slots(i).pDestOk  := (slots(i).inst.aRegsIdx.dest === 0.U) //default
    slots(i).rsReady  := false.B //default
  })

  //deal with rsReady
  val mainAluSel = getMainALUSlot()
  val subAluSel  = getSubALUSlot()
  val lsuSel     = getRsSel(ChiselFuType.LSU.asUInt)
  val mduSel     = getRsSel(ChiselFuType.MDU.asUInt)
  val rsSlotSel  = List(mainAluSel, subAluSel, lsuSel, mduSel)
  val toRs       = List(io.out.toMainAluRs, io.out.toSubAluRs, io.out.toLsuRs, io.out.toMduRs)
  List.tabulate(dispatchNum)(i => {
    slots(i).rsReady := Mux1H(rsSlotSel.map(_(i)), toRs.map(_.ready)) & (rsSlotSel.map(_(i))).asUInt.orR
  })

  //deal with pDestOk
  val needPdest = WireInit(VecInit((0 until dispatchNum).map(i => (slots(i).inst.aRegsIdx.dest =/= 0.U))))
  (0 until dispatchNum).map(i => {
    when(needPdest(i)) {
      slots(i).toRsBasic.destPregAddr := Mux1H(
        CountMask.oneHot(needPdest.asUInt(i, 0)),
        (0 to i).map(freeList.io.pop(_).bits)
      )
      slots(i).pDestOk := Mux1H(CountMask.oneHot(needPdest.asUInt(i, 0)), (0 to i).map(freeList.io.pop(_).valid))
    }
  })

  /**
    * deal with mispre block
    *
    * notice t0 and t1 can ↑ at one cycle
    *   t0:mispre
    *     don't block
    *   t1:ds get into rob
    *     redirect
    *     block...
    *   t2:srat-recoverd
    *     state back to normal,don't block
    *     but inst still can't go,because rob won't ready
    *   t3:fl recoverd
    */
  val realTargetReg = RegInit(0.U(vaddrWidth.W))
  addSource(io.fronRedirect.flush, "MisPredFrontRedirct")
  asg(io.fronRedirect.flush, false.B) //default
  asg(io.fronRedirect.target, realTargetReg) //default

  object DispatcherState extends ChiselEnum {
    val normal, waitDs, block = Value
  }
  import DispatcherState._
  val state  = RegInit(normal)
  val mispre = io.fromAluMispre
  switch(state) {
    is(normal) {
      when(mispre.happen) {
        when(!io.dsAllow) {
          asg(state, waitDs)
          asg(realTargetReg, mispre.realTarget)
        }.otherwise {
          asg(state, block)
          asg(io.fronRedirect.flush, true.B)
          asg(io.fronRedirect.target, mispre.realTarget)
        }
      }
    }
    is(waitDs) {
      when(io.dsAllow) {
        asg(state, block)
        asg(io.fronRedirect.flush, true.B)
        asg(io.fronRedirect.target, realTargetReg)
      }
    }
    is(block) {
      when(io.recoverSrat.valid) { asg(state, normal) }
    }
  }
  when(io.in.flushBackend) { asg(state, normal) }

  //deal with readyGo
  slots(0).readyGo := slots(0).robReady && slots(0).pDestOk && slots(0).rsReady && state =/= block
  (1 until dispatchNum).map(i => {
    slots(i).readyGo :=
      slots(i).robReady && slots(i).pDestOk && slots(i).rsReady &&
        slots(i - 1).readyGo
  })

  //io.out.toRob(i).fire === slots(i).out.fire
  io.outFireNum := PriorityCount((0 until dispatchNum).map(io.out.toRob(_).fire))

  //decoder
  List.tabulate(dispatchNum)(i => {
    decoder(i).io.in.instr      := slots(i).inst.basicInstInfo.instr
    decoder(i).io.in.exception  := slots(i).inst.exception
    slots(i).toRsBasic.exDetect := decoder(i).io.out.exception
    slots(i).decoded            := decoder(i).io.out.decoded
  })

  //srat rename
  srat.io.wb <> io.in.fuWbSrat
  srat.io.recover <> io.recoverSrat
  val slotsAregsIdx = WireInit(VecInit((0 until dispatchNum).map(i => slots(i).inst.aRegsIdx)))
  val slotsRenamed  = srat.read(aRegsIdx = slotsAregsIdx)
  List.tabulate(dispatchNum)(i => {
    srat.io.dest(i).currPDest.bits  := slots(i).toRsBasic.destPregAddr
    srat.io.dest(i).currPDest.valid := io.out.toRob(i).fire //toRob.fire==slot(i).fire
    slots(i).toRsBasic.srcPregs     := slotsRenamed(i)._1
    slots(i).prevPDest              := slotsRenamed(i)._2
  })

  //to rob
  List.tabulate(dispatchNum)(i => {
    val toRobBits    = io.out.toRob(i).bits
    val toRobUop     = toRobBits.uOp
    val toRobExBasic = toRobBits.basicExInfo
    asg(io.out.toRob(i).valid, slots(i).valid & slots(i).readyGo)
    //exception:pc and isBd
    asg(toRobExBasic.pc, slots(i).inst.basicInstInfo.pcVal)
    asg(toRobExBasic.isBd, slots(i).inst.isBd)
    //uOp
    asg(toRobUop.prevPDest, slots(i).prevPDest)
    asg(toRobUop.currADest, slots(i).inst.aRegsIdx.dest)
    asg(toRobUop.currPDest, slots(i).toRsBasic.destPregAddr)
    asg(toRobUop.specialType, decoder(i).io.out.decoded.specialType)
    //noBrMis
    val preTaken       = slots(i).inst.predictResult.taken
    val noBrMisPredict = preTaken && slots(i).inst.realBrType === BranchType.NON
    asg(toRobBits.isNoBrMis, noBrMisPredict)
    asg(toRobBits.isFirPreTake, slots(i).inst.isFirPreTake)
  })

  //to fl
  val allowFlPopMask = CountMask.apply((0 until dispatchNum).map(i => needPdest(i) & io.out.toRob(i).fire).asUInt)
  List.tabulate(dispatchNum)(i => {
    freeList.io.pop(i).ready := allowFlPopMask(i)
  })

  //to rs
  //rs is special
  val rsKind = List(FuType.MainAlu, FuType.SubAlu, FuType.Lsu, FuType.Mdu)
  List.tabulate(toRs.length)(i => {

    val thisSlot = Mux1H(rsSlotSel(i), (0 until dispatchNum).map(slots(_)))
    val toRsBits = toRs(i).bits
    toRs(i).valid  := false.B //only valid is important
    toRsBits       := DontCare //init(for extra)
    toRsBits.basic := thisSlot.toRsBasic

    when(rsSlotSel(i).orR) {
      toRs(i).valid := thisSlot.valid & thisSlot.readyGo
      if (rsKind(i) == FuType.MainAlu) {
        val maExtra = toRsBits.mAluExtra.get
        val maInst  = thisSlot.inst
        val basic   = maInst.basicInstInfo
        val preRes  = maInst.predictResult
        asg(maExtra.pcVal, basic.pcVal)
        asg(maExtra.low26, basic.instr(25, 0))
        asg(maExtra.predictResult, preRes)
        asg(toRsBits.uOp.aluType.get, thisSlot.decoded.aluType)
        asg(toRsBits.uOp.brType.get, thisSlot.inst.realBrType)
      }
      if (rsKind(i) == FuType.Mdu) {
        val instr = thisSlot.inst.basicInstInfo.instr
        asg(toRs(i).bits.c0Addr.get, Cat(instr(15, 11), instr(2, 0)))
        asg(toRsBits.uOp.mduType.get, thisSlot.decoded.mduType)
      }
      if (rsKind(i) == FuType.Lsu) {
        val instr = thisSlot.inst.basicInstInfo.instr
        val imm   = instr(15, 0)
        asg(toRs(i).bits.immOffset.get, imm)
        asg(toRsBits.uOp.memType.get, thisSlot.decoded.memType)
        asg(toRsBits.cacheOp.get, CacheOp.safe(instr(20, 16))._1)
      }
      if (rsKind(i) == FuType.SubAlu) {
        val instr = thisSlot.inst.basicInstInfo.instr
        val imm   = instr(15, 0)
        asg(toRs(i).bits.immOffset.get, imm)
        asg(toRsBits.uOp.aluType.get, thisSlot.decoded.aluType)
      }
    }
  })
}
