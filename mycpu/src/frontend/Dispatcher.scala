package frontend
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid

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
      val robIndex        = Vec(renameNum, Flipped(Decoupled(Output(ROBIdx))))
    }
    val out = new Bundle {
      //rs.in is just rs.out
      val toMainAluRs = Decoupled(new RsOutIO(kind = FuType.MainAlu))
      val toSubAluRs  = Decoupled(new RsOutIO(kind = FuType.SubAlu))
      val toMduRs     = Decoupled(new RsOutIO(kind = FuType.Mdu))
      val toLsuRs     = Decoupled(new RsOutIO(kind = FuType.Lsu))
      val toRob       = Vec(dispatchNum, Decoupled(new DispatchToRobBundle))
    }
  })
}
