package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.asg

class WakeUpBroadCast extends MycpuBundle {
  val (fromMainAluIs, fromSubAluIs, fromMainAluRo, fromSubAluRo, fromLsu) =
    (Valid(PRegIdx), Valid(PRegIdx), Valid(PRegIdx), Valid(PRegIdx), Valid(PRegIdx))
}

/**
  * allocate → writeIn - ready - select → readOp
  *
  * in.fromDispatcher.ready = rs not full
  *
  * in rename stage,we select a slot
  *
  * writeIn
  *     reuse rsOutIO for port <fromDispatcher>
  *     because the info written into Rs all need to take to FU
  *
  * ready
  *     1.already rdy in renameStage
  *     2.listen to wenPRF...next cycle the rdy bit will ↑
  *     3.wake-up：the selected insts broadCast its destPregAddr
  *     inte：
  *         msAlu -> otherRS (ReadOp)<1 bubble>
  *         mAlu<->sAlu (when "selected")<no bubble,need bypass>
  *         abandon now:load -> otherRS (MemStage1)<2 bubble>
  *     intra：
  *         mAlu<->mAlu
  *         sAlu<->sAlu
  *         (when "selected")<no bubble,need bypass>
  *
  * select
  *     use priority to select ready slot
  *         LSU/MDU/br：not any older insts(in-order)
  *         ALU(not br)：not any rdy&older insts(ooo)
  *     the selected insts broadCast its destPregAddr ,and will leave RS the next cycle  when fire()
  *
  * out
  *     RS keep the info that the insts will use in the FU
  *     decoded：srcAreg used in RO,uOps used in EXE,destAreg for s-rat update
  *     exception：record exception happended in FU,need write to ROB
  *     predictRes：br need it to detect mispredict in EXE
  *
  *     use sEntry(psrc+valid) in RO,use pDest in WB
  *     use robIndex in WB
  */
class RS(rsKind: FuType.t, rsSize: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromDispatcher = Flipped(Decoupled(new RsOutIO(rsKind)))
      val wPrfPIdx       = Vec(wBNum, Flipped(Valid(PRegIdx)))
      val flush          = Input(Bool()) //mispredict retire,exception,eret
      val oldestRobIdx   = Input(ROBIdx)
      val stqEmpty       = Input(Bool())
    }
    val out = Decoupled(new RsRealOutIO(rsKind))
  })
  val outBits   = io.out.bits
  val originOut = outBits.origin

  val rsEntries  = Reg(Vec(rsSize, new RsOutIO(rsKind)))
  val slotsValid = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val deqSel     = Wire(Vec(rsSize, Bool())) //one-hot or all-zero
  val enqSlot    = WireInit(0.U(log2Up(rsSize).W)) //default

  val srcsWaken = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B)))))
  val mayNeedBp = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B)))))
  val inPrf     = WireInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B)))))
  val src1Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => inPrf(i)(0) | srcsWaken(i)(0))))
  val src2Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => inPrf(i)(1) | srcsWaken(i)(1))))
  (0 until rsSize).map(i => {
    val rsB = rsEntries(i).basic
    (0 until srcDataNum).map(j => {
      val beenWb = WireInit(VecInit(List.tabulate(wBNum)(k => rsB.wbInfo(k) === rsB.pSrcs(j)))).asUInt.orR
      inPrf(i)(j) := rsB.grpInPrf(j) & (beenWb | rsB.sratInPrf(j))
      //inPrf(i)(j) := rsB.grpInPrf(j) & (rsB.wbInPrf(j) | rsB.sratInPrf(j))
    })
  })

  val isOldestVec = (0 until rsSize).map(i => rsEntries(i).basic.robIndex === io.in.oldestRobIdx)
  val blockVec    = WireInit(VecInit(Seq.fill(rsSize)(false.B)))
  if (rsKind == FuType.Lsu) {
    import MemType._
    (0 until rsSize).map(i =>
      asg(
        blockVec(i),
        rsEntries(i).uOp.memType.get.isOneOf(CACHEINST, SC, LL, LWL, LWR)
      )
    )
  }
  if (rsKind == FuType.Mdu) {
    import MduType._
    (0 until rsSize).map(i => asg(blockVec(i), rsEntries(i).uOp.mduType.get.isOneOf(MFC0, TLBP, TLBR, TLBWI, TLBWR)))
  }
  if (rsKind == FuType.MainAlu) {
    import AluType._
    (0 until rsSize).map(i => {
      val aluType = rsEntries(i).uOp.aluType.get
      asg(blockVec(i), aluType.isOneOf(MOVN, MOVZ))
    })
  }

  val slotsRdy = Wire(Vec(rsSize, Bool()))
  (0 until rsSize).foreach(i => {
    val releaseCond = if (rsKind == FuType.Lsu) io.in.stqEmpty && isOldestVec(i) else isOldestVec(i)
    slotsRdy(i) := src1Rdy(i) && src2Rdy(i) && slotsValid(i) && Mux(blockVec(i), releaseCond, true.B)
  })

  /**
    * ageMask:
    *   attention:in and out fire together
    *   当outfire时，slotsvalid下一拍才会拉低
    *   这一拍进来的slot的那一行agemask对应deqslot的那一位需要是false
    */
  val ageMask = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(rsSize)(false.B)))))
  when(io.in.fromDispatcher.fire) { ageMask(enqSlot) := slotsValid }
  when(io.out.fire) {
    ageMask.foreach(msk => {
      (0 until rsSize).map(idx =>
        when(deqSel(idx)) {
          asg(msk(idx), false.B)
        }
      )
    })
  }

  //wake-up
  val wakeUpSource = Wire(Valid(PRegIdx))
  wakeUpSource.bits  := originOut.basic.destPregAddr
  wakeUpSource.valid := io.out.fire && wakeUpSource.bits.orR
  if (rsKind == FuType.MainAlu) {
    val outAluType = originOut.uOp.aluType.get
    when(outAluType === AluType.MOVN || outAluType === AluType.MOVZ) {
      wakeUpSource.valid := false.B //这两条指令会在ro阶段停两拍，暂时不将他们作为唤醒源
    }
  }

  val wakeUpReceive = Wire(new WakeUpBroadCast)
  wakeUpReceive                     := DontCare
  wakeUpReceive.fromLsu.valid       := false.B
  wakeUpReceive.fromMainAluIs.valid := false.B
  wakeUpReceive.fromSubAluIs.valid  := false.B
  wakeUpReceive.fromMainAluRo.valid := false.B
  wakeUpReceive.fromSubAluRo.valid  := false.B

  if (rsKind == FuType.MainAlu) {
    BoringUtils.addSink(wakeUpReceive.fromSubAluIs, "sAluIsWakeUp") //receive
    BoringUtils.addSink(wakeUpReceive.fromLsu, "LsuM1WakeUp") //receive
    wakeUpReceive.fromMainAluIs := wakeUpSource
    BoringUtils.addSource(wakeUpSource, "mAluIsWakeUp")
  } else if (rsKind == FuType.SubAlu) {
    BoringUtils.addSink(wakeUpReceive.fromMainAluIs, "mAluIsWakeUp") //receive
    BoringUtils.addSink(wakeUpReceive.fromLsu, "LsuM1WakeUp") //receive
    wakeUpReceive.fromSubAluIs := wakeUpSource
    BoringUtils.addSource(wakeUpSource, "sAluIsWakeUp")
  } else if (rsKind == FuType.Lsu) {
    BoringUtils.addSink(wakeUpReceive.fromSubAluRo, "sAluRoWakeUp")
    BoringUtils.addSink(wakeUpReceive.fromMainAluRo, "mAluRoWakeUp")
    BoringUtils.addSink(wakeUpReceive.fromLsu, "LsuM1WakeUp")
    //BoringUtils.addSink(wakeUpReceive.fromSubAluIs, "sAluIsWakeUp")
    //BoringUtils.addSink(wakeUpReceive.fromMainAluIs, "mAluIsWakeUp")
  } else {
    BoringUtils.addSink(wakeUpReceive.fromSubAluRo, "sAluRoWakeUp")
    BoringUtils.addSink(wakeUpReceive.fromMainAluRo, "mAluRoWakeUp")
  }

  val wakeUpByPass = List(
    wakeUpReceive.fromMainAluIs,
    wakeUpReceive.fromSubAluIs,
    wakeUpReceive.fromLsu
  )
  val wakeUpBroad = List(
    wakeUpReceive.fromMainAluRo,
    wakeUpReceive.fromSubAluRo,
    wakeUpReceive.fromMainAluIs, //bypass
    wakeUpReceive.fromSubAluIs, //bypass
    wakeUpReceive.fromLsu //bypass
  )
  wakeUpBroad.foreach(e =>
    List.tabulate(rsSize)(j => {
      val pSrcs = rsEntries(j).basic.pSrcs
      when(slotsValid(j) && e.valid && e.bits.orR) {
        when(e.bits === pSrcs(0)) { srcsWaken(j)(0) := true.B }
        when(e.bits === pSrcs(1)) { srcsWaken(j)(1) := true.B }
      }
    })
  )
  wakeUpByPass.foreach(e =>
    List.tabulate(rsSize)(j => {
      val pSrcs = rsEntries(j).basic.pSrcs
      when(slotsValid(j) && e.valid && e.bits.orR) {
        when(e.bits === pSrcs(0)) { mayNeedBp(j)(0) := true.B }
        when(e.bits === pSrcs(1)) { mayNeedBp(j)(1) := true.B }
      }
    })
  )

  /**
    * rs enqueue
    * enq not zip,fill in minIndex notValid Slot
    */
  //(log2Up(rsSize + 1).W)
  val rsFull = slotsValid.asUInt.andR
  io.in.fromDispatcher.ready := ~rsFull
  val emptySlot = ~slotsValid.asUInt
  when(~rsFull) { asg(enqSlot, PriorityEncoder(emptySlot)) }
  when(io.in.fromDispatcher.fire) {
    rsEntries(enqSlot)  := io.in.fromDispatcher.bits
    slotsValid(enqSlot) := true.B
  }

  /**
    * rs dequeue
    *   must slotsRdy
    *   priority:
    *     older not ready/not any older
    *     bru is special
    */
  asg(io.out.valid, deqSel.asUInt.orR)

  if (rsKind == FuType.Mdu || rsKind == FuType.Lsu) {
    (0 until rsSize).map(i => asg(deqSel(i), !(ageMask(i).asUInt.orR) && slotsRdy(i)))
  }
  if (rsKind == FuType.SubAlu) {
    (0 until rsSize).map(i => asg(deqSel(i), !((ageMask(i).asUInt & slotsRdy.asUInt).orR) && slotsRdy(i)))
  }
  if (rsKind == FuType.MainAlu) {
    val isBranch =
      WireInit(
        VecInit(List.tabulate(rsSize)(i => rsEntries(i).uOp.brType.get =/= BranchType.NON && slotsValid(i)))
      )
    (0 until rsSize).map(i =>
      asg(
        deqSel(i),
        !((ageMask(i).asUInt & slotsRdy.asUInt).orR) && slotsRdy(i) && !(isBranch(i) & ((ageMask(
          i
        ).asUInt & isBranch.asUInt).orR))
      )
    )
  }

  when(io.out.fire) {
    assert(PopCount(deqSel) === 1.U)
  }
  originOut         := Mux1H(deqSel, rsEntries)
  outBits.mayNeedBp := Mux1H(deqSel, mayNeedBp)
  outBits.inPrf     := Mux1H(deqSel, inPrf)
  when(io.out.fire) {
    (0 until rsSize).foreach(i => {
      when(deqSel(i)) {
        asg(slotsValid(i), false.B)
        asg(srcsWaken(i)(0), false.B)
        asg(srcsWaken(i)(1), false.B)
        asg(mayNeedBp(i)(0), false.B)
        asg(mayNeedBp(i)(1), false.B)
      }
    })
  }
  //listen to wPrfPIdx
  val outNeedBp = outBits.mayNeedBp
  val outInPrf  = outBits.inPrf
  List.tabulate(wBNum)(i =>
    List.tabulate(rsSize)(j => {
      val wprf = io.in.wPrfPIdx(i)
      val rsB  = rsEntries(j).basic
      val srcs = rsB.pSrcs
      when(wprf.valid && slotsValid(j)) {
        (0 until srcDataNum).map(k => {
          when(wprf.bits === srcs(k)) {
            rsB.grpInPrf(k)  := true.B
            rsB.sratInPrf(k) := true.B
            rsB.wbInPrf(k)   := true.B
            mayNeedBp(j)(k)  := false.B
            when(deqSel(j)) {
              outInPrf(k)  := true.B
              outNeedBp(k) := false.B //暂时没用上，但先加上
            }
          }
        })
      }
    })
  )

  //flush
  when(io.in.flush) {
    List.tabulate(rsSize)(i => {
      ageMask(i)    := VecInit(Seq.fill(rsSize)(false.B))
      slotsValid(i) := false.B
    })
    srcsWaken := VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B))))
    mayNeedBp := VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B))))
  }
}
