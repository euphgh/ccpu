package frontend
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid
import utils.MultiQueue
import chisel3.util.log2Up
import utils._
import chisel3.util.Cat

class RATWriteBackIO extends MycpuBundle {
  val aDest = ARegIdx
  val pDest = PRegIdx
}

/**
  * wb is used to update inPrf bit
  * need check SRAT(wbADest).pIdx === wbPDest
  * if same, write inPrf := true in next cycle, else not change.
  * if same, and at this cycle src(of a renaming inst) = this AReg,
  * SRAT should return inPrf === true
  *
  * //Q:not need condition "if same?"
  * if same, and at this cycle dest(of a renaming inst) = this AReg,
  * next cycle inPrf := false
  * in total,  src < wb < dest
  */

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
    val wb      = Vec(retireNum, Flipped(Valid(new RATWriteBackIO)))
    val recover = Flipped(Valid(Vec(aRegNum, new SRATEntry)))
  })

  //areg0 -> (0,true)
  val pIdxMap = RegInit(VecInit((0 until aRegNum).map(i => i.U)))
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
  List.tabulate(retireNum)(i =>
    when(io.wb(i).valid & (pIdxMap(io.wb(i).bits.aDest) === io.wb(i).bits.pDest)) {
      inPrf(io.wb(i).bits.aDest) := true.B
      List.tabulate(renameNum)(i => {
        List.tabulate(srcDataNum)(j =>
          when(io.wb(i).bits.aDest === io.src(i).in(j)) { io.src(i).out(j).inPrf := true.B }
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
            io.dest(j).prevPDest := io.dest(i).currPDest
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
      val inst      = UWord
      val exception = FrontExcCode()
    }
    val out = new Bundle {
      val decoded   = new DecodeInstInfoBundle
      val exception = new ExceptionInfoBundle
    }
  })
}

class dispatchSlot extends MycpuBundle {
  val inst  = new InstBufferOutIO
  val valid = Output(Bool())

  val pDestOk  = Output(Bool()) // not need pdest/related fl slot pop valid
  val robReady = Output(Bool()) //rob是否有相应的空闲槽位，此处无需设置“leadingRobReady”
  val rsReady  = Output(Bool()) //指令对应的rs是否"对其"ready(注意同类型指令只有第一条才会“得到”Rdy)

  val readyGo = Output(Bool())

  //  exception decoder-Rs
  //  decoded   decoder-Rs
  //  destPregAddr freelist-Rs&Rob
  //  srcPregs     srat-Rs
  //  robIndex     Rs
  val toRsBasic = new RsBasicEntry
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
      val fromInstBuffer  = Vec(decodeNum, Flipped(Valid(new InstBufferOutIO)))
      val fromFuWriteBack = Vec(retireNum, Flipped(Valid(new RATWriteBackIO)))
      val robIndex        = Input(ROBIdx)
    }
    val outFireNum   = Output(UInt())
    val robEmpty     = Input(Bool())
    val stqEmpty     = Input(Bool())
    val isMispredict = Input(Bool())

    //valid when flush(mispredictRetire/exception/eret)
    val recoverSrat = Vec(aRegNum, Flipped(Valid(new SRATEntry)))

    val out = new Bundle {
      val toMainAluRs = Decoupled(new RsOutIO(kind = FuType.MainAlu))
      val toSubAluRs  = Decoupled(new RsOutIO(kind = FuType.SubAlu))
      val toMduRs     = Decoupled(new RsOutIO(kind = FuType.Mdu))
      val toLsuRs     = Decoupled(new RsOutIO(kind = FuType.Lsu))
      val toRob       = Vec(dispatchNum, Decoupled(new DispatchToRobBundle))
    }
  })

  //TODO:some inst can go to main/sub alurs;some can only goto sub alurs
  val noInst = (dispatchNum).U
  def getRsSlot(rsType: UInt): UInt = Mux(
    (slots(0).inst.whichFu === ChiselFuType(rsType)),
    0.U,
    Mux(
      slots(1).inst.whichFu === ChiselFuType(rsType),
      1.U,
      Mux(slots(2).inst.whichFu === ChiselFuType(rsType), 2.U, noInst)
    )
  )

  val freeListSize = 32
  val freeList = Module(
    new MultiQueue(enqNum = wBNum, deqNum = dispatchNum, gen = PRegIdx, size = freeListSize, allIn = false)
  )
  val decoder = List(Module(new Decoder), Module(new Decoder), Module(new Decoder))
  val srat    = Module(new SRAT)
  val slots   = Wire(Vec(dispatchNum, new dispatchSlot))

  //just connect(combinational logic)
  //input:ibf/rob/fl
  List.tabulate(dispatchNum)(i => {
    slots(i).inst  := io.in.fromInstBuffer(i).bits
    slots(i).valid := io.in.fromInstBuffer(i).valid

    slots(i).toRsBasic.destPregAddr := freeList.io.pop(i).bits
    slots(i).toRsBasic.robIndex     := io.in.robIndex + i.U

    slots(i).robReady := io.out.toRob(i).ready
    slots(i).pDestOk  := (slots(i).inst.aRegsIdx.dest === 0.U) //default
    slots(i).rsReady  := false.B //default
  })

  //deal with rsReady
  val mainAluSlot = getRsSlot(ChiselFuType.MainALU.asUInt)
  val subAluSlot  = getRsSlot(ChiselFuType.ALU.asUInt)
  val lsuSlot     = getRsSlot(ChiselFuType.LSU.asUInt)
  val mduSlot     = getRsSlot(ChiselFuType.MDU.asUInt)
  val rsSlotSel   = List(mainAluSlot, subAluSlot, lsuSlot, mduSlot)
  val toRs        = List(io.out.toMainAluRs, io.out.toSubAluRs, io.out.toLsuRs, io.out.toMduRs)
  List.tabulate(rsSlotSel.length)(i => {
    when(rsSlotSel(i) =/= noInst) {
      slots(rsSlotSel(i)).rsReady := toRs(i).ready
    }
  })

  //deal with pDestOk
  val needPdest    = WireInit(VecInit((0 until dispatchNum).map(i => (slots(i).inst.aRegsIdx.dest =/= 0.U))))
  val cntNeedPdest = Wire(Vec(dispatchNum, UInt(log2Up(dispatchNum).W)))
  cntNeedPdest(0) := 0.U
  (1 to dispatchNum).map(i => { cntNeedPdest(i) := cntNeedPdest(i - 1) +& needPdest(i - 1).asUInt }) //cntNeedPdest是总数
  (0 until dispatchNum).map(i => when(needPdest(i)) { slots(i).pDestOk := freeList.io.pop(cntNeedPdest(i)).valid })

  //blockReg,be aware of priority
  val blockReg      = RegInit(false.B)
  val pipelineEmpty = io.robEmpty && io.stqEmpty
  when(io.isMispredict) { blockReg := true.B }
  when(pipelineEmpty) { blockReg := false.B }

  //deal with readyGo
  val firBlkType = decoder(0).io.out.decoded.blockType
  slots(0).readyGo :=
    slots(0).robReady && slots(0).pDestOk && slots(0).rsReady &&
      !((firBlkType === BlockType.CACHEINST && !pipelineEmpty) || (firBlkType === BlockType.MFC0 && !io.robEmpty)) &&
      !io.isMispredict &&
      !(blockReg && !pipelineEmpty)
  (1 until dispatchNum).map(i => {
    slots(i).readyGo :=
      slots(i).robReady && slots(i).rsReady && slots(i).pDestOk &&
        slots(i - 1).readyGo &&
        decoder(i).io.out.decoded.blockType === BlockType.NON &&
        !io.isMispredict &&
        !(blockReg && pipelineEmpty)
  })

  //io.out.toRob(i).fire === slots(i).out.fire
  io.outFireNum := List.tabulate(dispatchNum)(i => { io.out.toRob(i).fire }).foldRight(0.U)((sum, i) => sum.asUInt +& i)

  //decoder
  List.tabulate(dispatchNum)(i => {
    decoder(i).io.in             := slots(i).inst.basic.instr
    decoder(i).io.in.exception   := slots(i).inst.exception
    slots(i).toRsBasic.decoded   := decoder(i).io.out.decoded
    slots(i).toRsBasic.exception := decoder(i).io.out.exception
  })

  //srat rename
  srat.io.wb <> io.in.fromFuWriteBack
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
    asg(io.out.toRob(i).valid, slots(i).valid & slots(i).readyGo)
    asg(io.out.toRob(i).bits.pc, slots(i).inst.basic.pcVal)
    asg(io.out.toRob(i).bits.prevPDest, slots(i).prevPDest)
    asg(io.out.toRob(i).bits.currADest, slots(i).inst.aRegsIdx.dest)
    asg(io.out.toRob(i).bits.currPDest, slots(i).toRsBasic.destPregAddr)
    asg(io.out.toRob(i).bits.specialType, decoder(i).io.out.decoded.specialType)
  })

  //to fl
  val allowFlPopNum =
    (0 until dispatchNum).map(i => needPdest(i) & io.out.toRob(i).fire).foldRight(0.U)((sum, i) => sum.asUInt +& i)
  List.tabulate(dispatchNum)(i => {
    freeList.io.pop(i).ready := (i.U < allowFlPopNum)
  })

  //to rs
  //rs is special
  List.tabulate(toRs.length)(i => {
    when(rsSlotSel(i) === noInst) {
      toRs(i).valid := false.B
      toRs(i).bits  := DontCare
    }.otherwise {
      toRs(i).valid      := slots(rsSlotSel(i)).valid & slots(rsSlotSel(i)).readyGo
      toRs(i).bits.basic := slots(rsSlotSel(i)).toRsBasic
    }
  })
  //已经考虑===noInst的情况 =DontCare
  when(mainAluSlot =/= noInst) {
    io.out.toMainAluRs.bits.predictResult.get := slots(mainAluSlot).inst.predictResult
  }
  when(lsuSlot =/= noInst) {
    asg(io.out.toLsuRs.bits.memInstOffset.get, slots(lsuSlot).inst.basic.instr(memInstOffsetWidth - 1, 0))
  }
}
