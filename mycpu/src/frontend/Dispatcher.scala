package frontend
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid

/**
  * not solve WAW/RAW conflict
  * only read/write data by index
  */
class SRATEntry extends MycpuBundle {
  val pIdx  = PRegIdx
  val inPrf = Bool
}

class SRATWriteBackIO extends MycpuBundle {
  val wbADest = ARegIdx
  val wbPDest = PRegIdx
}

/**
  * wb is used to update inPrf bit
  * need check SRAT(wbADest).pIdx === wbPDest
  * if same, write inPrf := true in next cycle, else not change.
  * if same, and at this cycle src read this AReg,
  * SRAT should return inPrf === true
  * if same, and at this cycle dest write this AReg,
  * next cycle inPrf := false
  * in total,  src < wb < dest
  */
class SRAT extends MycpuModule {
  val io = IO(new Bundle {
    val src = List(
      renameNum,
      new Bundle {
        val in  = Output(Vec(srcDataNum, ARegIdx))
        val out = Output(Vec(srcDataNum, new SRATEntry))
      }
    )
    val dest = Vec(
      renameNum,
      Valid(new Bundle {
        val currADest = Input(ARegIdx)
        val currPDest = Input(PRegIdx)
        val out       = Output(new SRATEntry)
      })
    )
    val wb = Vec(retireNum, Flipped(Valid(new SRATWriteBackIO)))
  })
  def read(channel: Int, aRegsIdx: Seq[InstARegsIdxIO]) = {
    require(aRegsIdx.size == renameNum)
  }
}

class DecodeOutIO extends MycpuBundle {}
class Decoder extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val inst      = UWord
      val exception = FrontExcCode()
    }
    val out = new DecodeOutIO
  })
}

class RSDispatchInput extends MycpuBundle {
  val pc            = UWord
  val srcs          = Vec(srcDataNum, new SRATEntry)
  val pDest         = PRegIdx // use when write prf
  val aDest         = ARegIdx // use when write prf and update SRAT
  val decode        = new DecodeOutIO
  val renamed       = new RenameInfoBundle
  val exception     = new ExceptionInfoBundle
  val predictResult = new PredictResultBundle
}
class RobDispatchInput extends MycpuBundle {
  val pc          = UWord // difftest check execution flow
  val prevPRegIdx = PRegIdx // free when retire
  val currPRegIdx = PRegIdx // updata A-RAT when retire
  val currARegIdx = ARegIdx // updata A-RAT when retire
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
      val fromRSwriteback = Vec(retireNum, Flipped(Valid(new SRATWriteBackIO)))
    }
    val out = Vec(renameNum, Decoupled(new RSDispatchInput))
    val rob = Vec(
      renameNum,
      Decoupled(new Bundle {
        val out = new RobDispatchInput
        val in  = ROBIdx
      })
    )
  })
}
