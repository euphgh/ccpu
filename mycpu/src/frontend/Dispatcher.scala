package frontend
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid
import chisel3.util.log2Up
import utils.MultiQueue
import chisel3.util.RegEnable

class SRATWriteBackIO extends MycpuBundle {
  val wbADest = ARegIdx
  val wbPDest = PRegIdx
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
class SRAT extends MycpuModule {
  val io = IO(new Bundle {
    val src = List(
      renameNum,
      new Bundle {
        val in  = Input(Vec(srcDataNum, ARegIdx))
        val out = Output(Vec(srcDataNum, new SRATEntry))
      }
    )
    val dest = Vec(
      renameNum,
      Valid(new Bundle {
        val currADest = Input(ARegIdx) //to get prev
        val currPDest = Input(PRegIdx) //to write in
        val out       = Output(new SRATEntry) //prev
        //Q:prevDest need inPrf info?
      })
    )
    val wb = Vec(retireNum, Flipped(Valid(new SRATWriteBackIO)))
  })
  def read(channel: Int, aRegsIdx: Seq[InstARegsIdxBundle]) = {
    require(aRegsIdx.size == renameNum)
  }
}

//combination logic decode
class Decoder extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val inst      = UWord
      val exception = FrontExcCode()
    }
    val out = new DecodeInstInfoBundle
  })
}

class dispatchSlot extends MycpuBundle {
  val inst  = new InstBufferOutIO
  val valid = Output(Bool())

  //这三个互相阻塞
  val freeListPopValid = Output(Bool()) //freeList pop valid
  val robReady         = Output(Bool()) //rob是否有相应的空闲槽位，此处无需设置“leadingRobReady”
  val rsReady          = Output(Bool()) //指令对应的rs是否"对其"ready(注意同类型指令只有第一条才会“得到”Rdy)

  val leadingRsReady = Output(Bool()) // andR: (0 to x).rsReady

  //这3个等价，但是想保证一套握手信号valid-rdy相互独立
  val readyGoFlPop = Output(Bool()) //(0 to i-1)flOK(省略)   (0 to i)rsRdy     (0 to i)robRdy(i)
  val readyGoRs    = Output(Bool()) //(0 to i)flOK(i)       (0 to i-1)rsRdy   (0 to i)robRdy(i)
  val readyGoRob   = Output(Bool()) //(0 to i)flOK(i)       (0 to i)rsRdy     (0 to i-1)robRdy(省略)
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
  * 4. cache inst, cp0 read inst
  * if 3 inst can not be rename and dispatch, ready for InstBuffer is set false
  * until 3 inst are dispatched, InstBuffer can pop 3 insts in next cycle
  *
  * Dispatcher should maintain a automat to distingush how many insts left
  * inst must be dispatched in order, so automat only have 4 state(3, 2, 1, 0)
  * rename WAW/RAW conflict should take automat state into account
  */
class Dispatcher extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromInstBuffer  = Vec(decodeNum, Flipped(Decoupled(new InstBufferOutIO)))
      val fromFuWriteBack = Vec(retireNum, Flipped(Valid(new SRATWriteBackIO)))
      val robIndex        = Input(ROBIdx)
    }
    val outFireNum = Output(UInt())

    val out = new Bundle {
      //Rs decoupled is not "pipeline decoupled"
      //can treat fire() as wen
      val toMainAluRs = Decoupled(new RsOutIO(kind = FuType.MainAlu))
      val toSubAluRs  = Decoupled(new RsOutIO(kind = FuType.SubAlu))
      val toMduRs     = Decoupled(new RsOutIO(kind = FuType.Mdu))
      val toLsuRs     = Decoupled(new RsOutIO(kind = FuType.Lsu))
      val toRob       = Vec(dispatchNum, Decoupled(new DispatchToRobBundle))
    }
  })

  val freeList = Module(new MultiQueue(enqNum = wBNum, deqNum = dispatchNum, gen = PRegIdx))

  //TODO:some inst can go to main/sub alurs;some can only goto sub alurs
  val noInst = (dispatchNum).U
  def getSlot(rsType: UInt): UInt = Mux(
    (slots(0).inst.whichFu === ChiselFuType(rsType)),
    0.U,
    Mux(
      slots(1).inst.whichFu === ChiselFuType(rsType),
      1.U,
      Mux(slots(2).inst.whichFu === ChiselFuType(rsType), 2.U, noInst)
    )
  )

  val slots = Wire(Vec(dispatchNum, new dispatchSlot))

  //just connect(combinational logic)
  List.tabulate(dispatchNum)(i => {
    slots(i).inst             := io.in.fromInstBuffer(i).bits
    slots(i).valid            := io.in.fromInstBuffer(i).valid
    slots(i).robReady         := io.out.toRob(i).ready
    slots(i).freeListPopValid := freeList.io.pop(i).valid
    slots(i).rsReady          := false.B //default
  })

  val mainAluSlot = getSlot(ChiselFuType.MainALU.asUInt)
  val subAluSlot  = getSlot(ChiselFuType.ALU.asUInt)
  val lsuSlot     = getSlot(ChiselFuType.LSU.asUInt)
  val mduSlot     = getSlot(ChiselFuType.MDU.asUInt)
  //indirect index
  val rsSlotSel = List(mainAluSlot, subAluSlot, lsuSlot, mduSlot)
  val toRs      = List(io.out.toMainAluRs, io.out.toSubAluRs, io.out.toLsuRs, io.out.toMduRs)
  List.tabulate(rsSlotSel.length)(i => {
    when(rsSlotSel(i) =/= noInst) {
      slots(rsSlotSel(i)).rsReady := toRs(i).ready
    }
  })

  slots(0).leadingRsReady := slots(0).rsReady
  slots(0).readyGoRs      := slots(0).robReady & slots(0).freeListPopValid
  List.tabulate(dispatchNum)(i => {
    if (i != 0) {
      slots(i).leadingRsReady := slots(i - 1).leadingRsReady & slots(i).rsReady
      slots(i).readyGoRs      := slots(i).robReady & slots(i - 1).leadingRsReady & slots(i).freeListPopValid
    }
    slots(i).readyGoRob   := slots(i).leadingRsReady & slots(i).freeListPopValid
    slots(i).readyGoFlPop := slots(i).leadingRsReady & slots(i).robReady
  })

  //io.out.toRob(i).fire indicate that the inst can certainly out at next cycle
  io.outFireNum := List.tabulate(dispatchNum)(i => { io.out.toRob(i).fire }).foldRight(0.U)((sum, i) => sum.asUInt +& i)

  List.tabulate(dispatchNum)(i => {
    //situation: 1cango 2cantgo 3empty(rdy not rdy),so need to gen real rdy in pipeConnect
    io.in.fromInstBuffer(i).ready := !slots(i).valid || io.out.toRob(i).fire

    io.out.toRob(i).valid    := slots(i).valid & slots(i).readyGoRob
    toRs(i).valid            := slots(i).valid & slots(i).readyGoRs
    freeList.io.pop(i).ready := slots(i).valid & slots(i).readyGoFlPop
  })

}
