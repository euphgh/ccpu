package utils

import chisel3._
import chisel3.util._

class DPBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Ceil(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class DPBundleAW[T <: Data](private val gen: T, set: Int) extends DPBundleA(set) {
  val data = Output(gen)

  def apply(data: T, setIdx: UInt) = {
    super.apply(setIdx)
    this.data := data
    this
  }
}

class DPBundleR[T <: Data](private val gen: T) extends Bundle {
  val data = Output(gen)
}

class DPReadBus[T <: Data](private val gen: T, val set: Int) extends Bundle {
  val req  = Decoupled(new DPBundleA(set))
  val resp = Flipped(new DPBundleR(gen))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class DPWriteBus[T <: Data](private val gen: T, val set: Int) extends Bundle {
  val req = Decoupled(new DPBundleAW(gen, set))

  def apply(valid: Bool, data: T, setIdx: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx)
    this.req.valid := valid
    this
  }
}

class DPTemplate[T <: Data](
  gen:         T,
  set:         Int,
  shouldReset: Boolean = false,
  holdRead:    Boolean = false,
  singlePort:  Boolean = false,
  writefirst:  Boolean = true)
    extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new DPReadBus(gen, set))
    val w = Flipped(new DPWriteBus(gen, set))
  })

  val wordType               = UInt(gen.getWidth.W)
  val array                  = SyncReadMem(set, wordType)
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState              = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when(resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet   := _resetSet
  }

  val (ren, wen) = (io.r.req.valid, io.w.req.valid || resetState)
  val realRen    = (if (singlePort) ren && !wen else ren)

  val setIdx    = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val wdata     = wdataword
  when(wen) { array.write(setIdx, wdata) }

  val rdata = (if (holdRead) ReadAndHold(array, io.r.req.bits.setIdx, realRen)
               else array.read(io.r.req.bits.setIdx, realRen)).asTypeOf(gen)

  if (writefirst) {
    val rReqIndexReg = RegNext(io.r.req.bits.setIdx)
    val rReqValidReg = RegNext(io.r.req.valid)
    val wReqIndexReg = RegNext(io.w.req.bits.setIdx)
    val wReqValidReg = RegNext(io.w.req.valid)
    val wReqDataReg  = RegNext(io.w.req.bits.data)
    val reqConf      = (rReqValidReg && wReqValidReg) && (rReqIndexReg === wReqIndexReg)
    val confData     = wReqDataReg
    require(io.w.req.bits.data.getWidth == rdata.getWidth)
    when(reqConf) { rdata := confData }
  }

  io.r.resp.data := rdata

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B
}

class DPTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new DPReadBus(gen, set)))
    val w = Flipped(new DPWriteBus(gen, set))
  })

  val ram = Module(new DPTemplate(gen, set, shouldReset, holdRead = false, singlePort = true))
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map {
    case r => {
      r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire))
    }
  }
}
