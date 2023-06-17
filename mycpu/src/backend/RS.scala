package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

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
    val out = Decoupled(new RsOutIO(kind = rsKind))
  })

  val rsEntries  = RegInit(VecInit(Seq.fill(rsSize)(0.U.asTypeOf(new RsOutIO(rsKind)))))
  val slotsValid = RegInit(VecInit(Seq.fill(rsSize)(false.B)))

  val srcsWaken = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(srcDataNum)(false.B)))))
  val src1Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.srcPregs(0).inPrf | srcsWaken(i)(0))))
  val src2Rdy   = WireInit(VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.srcPregs(1).inPrf | srcsWaken(i)(1))))
  val slotsRdy  = WireInit(VecInit(List.tabulate(rsSize)(i => src1Rdy(i) & src2Rdy(i) & slotsValid(i))))

  //ageMask:
  //attention:in and out fire together
  val ageMask = RegInit(VecInit(Seq.fill(rsSize)(VecInit(Seq.fill(rsSize)(false.B)))))
  when(io.in.fromDispatcher.fire) { ageMask(enqSlot) := slotsValid }
  when(io.out.fire) { (0 until rsSize).map(i => ageMask(i)(deqSlot) := false.B) }

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
  val wakeUpSource = Valid(PRegIdx)
  wakeUpSource.bits  := rsEntries(deqSlot).basic.destPregAddr
  wakeUpSource.valid := io.out.fire

  val wakeUpReceive = new WakeUpBroadCast
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
  List.tabulate(wakeUpBroad.length)(i =>
    List.tabulate(rsSize)(j => {
      srcsWaken(j)(0) := (wakeUpBroad(i).bits === rsEntries(j).basic.srcPregs(0).pIdx && wakeUpBroad(i).valid)
      srcsWaken(j)(1) := (wakeUpBroad(i).bits === rsEntries(j).basic.srcPregs(1).pIdx && wakeUpBroad(i).valid)
    })
  )

  /**
    * rs enqueue
    * enq not zip,fill in minIndex notValid Slot
    */
  val rsFull = slotsValid.asUInt.andR
  io.in.fromDispatcher.ready := ~rsFull
  val emptySlot = ~slotsValid.asUInt
  val enqSlot   = PriorityEncoder(emptySlot)
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
  io.out.valid := slotsRdy.asUInt.orR
  val deqSlot = Wire(UInt(log2Up(rsSize).W))
  if (rsKind == FuType.Mdu || rsKind == FuType.Lsu) {
    deqSlot := OHToUInt(List.tabulate(rsSize)(i => ~ageMask(i).asUInt.orR))
  }
  if (rsKind == FuType.SubAlu) {
    deqSlot := OHToUInt(List.tabulate(rsSize)(i => ~(ageMask(i).asUInt & slotsRdy.asUInt).orR))
  }
  if (rsKind == FuType.MainAlu) {
    val isBranch = WireInit(VecInit(List.tabulate(rsSize)(i => rsEntries(i).basic.decoded.isBr))) //TODO:fix decodeInfo
    deqSlot := OHToUInt(
      List.tabulate(rsSize)(i =>
        (~(ageMask(i).asUInt & isBranch.asUInt).orR & isBranch(i))
          | (~(ageMask(i).asUInt & slotsRdy.asUInt).orR & ~isBranch(i))
      )
    )
  }
  io.out.bits := rsEntries(deqSlot)
  when(io.out.fire) { slotsValid(deqSlot) := false.B }

  //flush
  when(io.in.flush) {
    List.tabulate(rsSize)(i => {
      ageMask(i)    := VecInit(Seq.fill(rsSize)(false.B))
      slotsValid(i) := false.B
    })
  }
}
