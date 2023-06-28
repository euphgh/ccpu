package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import utils.asg

class WakeUpBroadCast extends MycpuBundle {
  val (fromMainAlu, fromSubAlu, fromLsu) = (Valid(PRegIdx), Valid(PRegIdx), Valid(PRegIdx))
}

/** rsEntry:
  * basic
  *   exception
  *   decoded
  *   destPregAddr
  *   srcPregs
  *   robIndex
  * an option predictResult
  * note that valid is decouple from rsEntry
  */

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
      val wPrf           = Vec(wBNum, Flipped(Valid(PRegIdx)))
      val flush          = Input(Bool()) //mispredict retire,exception,eret
    }
    val out          = Decoupled(new RsOutIO(kind = rsKind))
    val oldestRobIdx = Input(ROBIdx)
  })

  val rsEntries  = RegInit(VecInit(Seq.fill(rsSize)(0.U.asTypeOf(new RsOutIO(rsKind)))))
  val slotsValid = RegInit(VecInit(Seq.fill(rsSize)(false.B)))
  val deqSel     = Wire(Vec(rsSize, Bool())) //one-hot or all-zero
  val enqSlot    = WireInit(0.U(log2Up(rsSize).W)) //default

  val srcsWaken = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B)))))
  val src1Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.srcPregs(0).inPrf | srcsWaken(i)(0))))
  val src2Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.srcPregs(1).inPrf | srcsWaken(i)(1))))

  val isOldestVec = (0 until rsSize).map(i => rsEntries(i).basic.robIndex === io.oldestRobIdx)
  val blockVec    = WireInit(VecInit(Seq.fill(rsSize)(false.B)))
  if (rsKind == FuType.Lsu) {
    (0 until rsSize).map(i => asg(blockVec(i), rsEntries(i).basic.decoded.memType === MemType.CACHEINST))
  }
  if (rsKind == FuType.Mdu) {
    (0 until rsSize).map(i => asg(blockVec(i), rsEntries(i).basic.decoded.mduType === MduType.MFC0))
  }

  val slotsRdy = WireInit(
    VecInit(
      List.tabulate(rsSize)(i => src1Rdy(i) & src2Rdy(i) & slotsValid(i) & !(blockVec(i) & !isOldestVec(i)))
    )
  )

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

  //listen to wPrf
  List.tabulate(wBNum)(i =>
    List.tabulate(rsSize)(j => {
      rsEntries(j).basic
        .srcPregs(0)
        .inPrf := (io.in.wPrf(i).bits === rsEntries(j).basic.srcPregs(0).pIdx && io.in.wPrf(i).valid)
      rsEntries(j).basic
        .srcPregs(1)
        .inPrf := (io.in.wPrf(i).bits === rsEntries(j).basic.srcPregs(1).pIdx && io.in.wPrf(i).valid)
    })
  )

  //wake-up
  val wakeUpSource = Wire(Valid(PRegIdx))
  wakeUpSource.bits  := io.out.bits.basic.destPregAddr
  wakeUpSource.valid := io.out.fire

  val wakeUpReceive = Wire(new WakeUpBroadCast)
  //BoringUtils.addSink(wakeUpReceive.fromLsu, "LsuMem1WakeUp")
  wakeUpReceive.fromLsu.valid := false.B
  wakeUpReceive.fromLsu.bits  := DontCare

  if (rsKind == FuType.MainAlu) {
    BoringUtils.addSink(wakeUpReceive.fromSubAlu, "sAluIsWakeUp")
    wakeUpReceive.fromMainAlu := wakeUpSource
    BoringUtils.addSource(wakeUpSource, "mAluIsWakeUp")
  } else if (rsKind == FuType.SubAlu) {
    BoringUtils.addSink(wakeUpReceive.fromMainAlu, "mAluIsWakeUp")
    wakeUpReceive.fromSubAlu := wakeUpSource
    BoringUtils.addSource(wakeUpSource, "sAluIsWakeUp")
  } else {
    BoringUtils.addSink(wakeUpReceive.fromSubAlu, "sAluRoWakeUp")
    BoringUtils.addSink(wakeUpReceive.fromMainAlu, "mAluRoWakeUp")
  }

  val wakeUpBroad = List(wakeUpReceive.fromMainAlu, wakeUpReceive.fromSubAlu, wakeUpReceive.fromLsu)
  wakeUpBroad.foreach(e =>
    List.tabulate(rsSize)(j => {
      val pSrcs = rsEntries(j).basic.srcPregs
      asg(srcsWaken(j)(0), e.bits === pSrcs(0).pIdx && e.valid)
      asg(srcsWaken(j)(1), e.bits === pSrcs(1).pIdx && e.valid)
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
        VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.decoded.brType =/= BranchType.NON))
      )
    (0 until rsSize).map(i =>
      asg(
        deqSel(i),
        ((!((ageMask(i).asUInt & isBranch.asUInt).orR) & isBranch(i))
          | (!((ageMask(i).asUInt & slotsRdy.asUInt).orR) & !isBranch(i))) && slotsRdy(i)
      )
    )
  }

  io.out.bits := Mux1H(deqSel, rsEntries)
  when(io.out.fire) {
    (0 until rsSize).foreach(i => {
      when(deqSel(i)) {
        asg(slotsValid(i), false.B)
      }
    })
  }

  //flush
  when(io.in.flush) {
    List.tabulate(rsSize)(i => {
      ageMask(i)    := VecInit(Seq.fill(rsSize)(false.B))
      slotsValid(i) := false.B
    })
  }
}
