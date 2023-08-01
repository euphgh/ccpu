package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import chisel3.util.experimental.BoringUtils._
import difftest.DifftestSpecRAS
import difftest.DifftestArchRAS
import difftest.DifftestLHTRead

class BtbOutIO extends MycpuBundle {
  val instType = BtbType()
  val target   = UWord
}
class PhtOutIO extends MycpuBundle {
  val cnt  = UInt(2.W)
  val take = Bool()
}

class RetAddrStack(spec: Boolean, size: Int) extends MycpuModule {
  class RecoverIO extends MycpuBundle {
    val stack = Vec(size, UWord)
    val ptr   = UInt(log2Ceil(size).W)
  }
  val io = IO(new Bundle {
    val push    = Flipped(Valid(UWord))
    val pop     = Input(Bool())
    val topData = Output(UWord)
  })
  require(isPow2(size))
  val maxTop = (size - 1).U
  val stack  = RegInit(VecInit.fill(size)(0.U(32.W)))
  val ptr    = RegInit(0.U(log2Ceil(size).W))

  val updatePtr = ptr
  asg(io.topData, stack(ptr - 1.U))
  if (spec) {
    val recoverData = Wire(new RecoverIO)
    addSink(recoverData, "RASRecoverData")
    val misPredFrontRedirct = Wire(Bool())
    addSink(misPredFrontRedirct, "MisPredFrontRedirct")
    val recoverValid = RegNext(misPredFrontRedirct)
    when(recoverValid) {
      asg(stack, recoverData.stack)
      asg(ptr, recoverData.ptr)
      asg(io.topData, recoverData.stack(recoverData.ptr - 1.U))
      asg(updatePtr, recoverData.ptr)
    }
    val diffSpecRAS = Module(new DifftestSpecRAS)
    asg(diffSpecRAS.io.clock, clock)
    asg(diffSpecRAS.io.en, io.pop || io.push.valid || recoverValid)
    asg(diffSpecRAS.io.pushData, io.push.bits)
    asg(diffSpecRAS.io.push, io.push.valid)
    asg(diffSpecRAS.io.pop, io.pop)
    asg(diffSpecRAS.io.topData, io.topData)
    asg(diffSpecRAS.io.flush, recoverValid)
  } else {
    val recoverData = Wire(new RecoverIO)
    addSource(recoverData, "RASRecoverData")
    asg(recoverData.stack, stack)
    asg(recoverData.ptr, ptr)

    val diffArchRAS = Module(new DifftestArchRAS)
    asg(diffArchRAS.io.clock, clock)
    asg(diffArchRAS.io.en, io.pop || io.push.valid)
    asg(diffArchRAS.io.pushData, io.push.bits)
    asg(diffArchRAS.io.push, io.push.valid)
    asg(diffArchRAS.io.pop, io.pop)
    asg(diffArchRAS.io.topData, io.topData)
  }
  when(io.push.valid) {
    asg(ptr, updatePtr + 1.U)
    stack(updatePtr) := io.push.bits
  }
  when(io.pop) {
    asg(ptr, updatePtr - 1.U)
  }
}

class BtbUpdateIO extends MycpuBundle {
  val tagIdx   = Input(UInt((32 - log2Ceil(IcachLineBytes)).W))
  val instrOff = Vec(4, Input(UInt(instrOffWidth.W)))
  val data     = Vec(fetchNum, Flipped(Valid(new BtbOutIO)))
}
class PhtUpdateIO extends MycpuBundle {
  val tagIdx   = Input(UInt((32 - log2Ceil(IcachLineBytes)).W))
  val instrOff = Vec(4, Input(UInt(instrOffWidth.W)))
  val data     = Vec(fetchNum, Flipped(Valid(new PhtOutIO)))
}

object LocHisTab {
  val idxWidth: Int = 4
  val clrWidth: Int = 4
  val hisWidth: Int = 9
  val lhtTagWidth = 32 - idxWidth - 4
  val lhtTagLsb   = 32 - lhtTagWidth
  val entriesNum  = math.pow(2, idxWidth).toInt
  val takeCntNum  = math.pow(2, hisWidth).toInt
  class LhtOutIO extends MycpuBundle {
    val take = Bool()
    val cnt  = UInt(clrWidth.W)
  }
  def calNextCnt(cnt: UInt, take: Bool): UInt = {
    val ioWidth = cnt.getWidth
    val res = MuxCase(
      Mux(take, cnt + 1.U, cnt - 1.U),
      Seq(
        cnt.andR -> Mux(take, cnt, cnt - 1.U),
        !cnt.orR -> Mux(take, 1.U, 0.U)
      )
    )
    require(res.getWidth == ioWidth)
    res
  }
  def getTag(address: UInt) = {
    require(address.getWidth == 32)
    val res = address(31, lhtTagLsb)
    require(res.getWidth == lhtTagWidth)
    res
  }
  def getIdx(address: UInt) = {
    require(address.getWidth == 32)
    val res = address(lhtTagLsb - 1, 4)
    require(res.getWidth == idxWidth)
    res
  }
}
class LocHisTab extends MycpuModule {
  import LocHisTab._
  val update = IO(new Bundle {
    val tagIdx   = Input(UInt((32 - log2Ceil(IcachLineBytes)).W))
    val instrOff = Input(Vec(4, UInt(instrOffWidth.W)))
    val data     = Flipped(Vec(fetchNum, Valid(Bool())))
  })

  val readAddr = List.fill(fetchNum)(IO(Flipped(Valid(UWord))))
  val readRes  = List.fill(fetchNum)(IO(Output(new LhtOutIO)))

  val writePC = Wire(UWord)
  asg(writePC, Cat(update.tagIdx, update.instrOff(0), 0.U(2.W)))

  val diffLht = Module(new DifftestLHTRead)
  diffLht.io.clock := clock
  diffLht.io.en    := readAddr(0).valid
  asg(diffLht.io.outOK, RegNext(readAddr(0).valid))
  assert(readAddr(1).valid === readAddr(0).valid)
  assert(readAddr(2).valid === readAddr(0).valid)
  assert(readAddr(3).valid === readAddr(0).valid)
  (0 until fetchNum).foreach(i => {
    val readPC = RegEnable(readAddr(i).bits, readAddr(i).valid)
    asg(diffLht.io.readAddr(i), readAddr(i).bits)
    asg(diffLht.io.readCnt(i), readRes(i).cnt)
    asg(diffLht.io.readTake(i), readRes(i).take)
    val rPCIdx = getIdx(readPC)
    val wPCIdx = WireInit(getIdx(writePC))
    // write when valid
    val clrMem   = Mem(entriesNum, UInt(clrWidth.W))
    val clrROut  = clrMem.read(rPCIdx)
    val clrWOut  = clrMem.read(wPCIdx)
    val clrWen   = WireInit(false.B)
    val clrWData = WireInit(0.U(clrWidth.W))
    when(clrWen) { clrMem.write(wPCIdx, clrWData) }

    val tagsMem   = Mem(entriesNum, UInt(lhtTagWidth.W))
    val tagsROut  = tagsMem.read(rPCIdx)
    val tagsWOut  = tagsMem.read(wPCIdx)
    val tagsWen   = WireInit(false.B)
    val tagsWData = WireInit(0.U(lhtTagWidth.W))
    when(tagsWen) { tagsMem.write(wPCIdx, tagsWData) }

    val fastCntMem   = Mem(entriesNum, UInt(2.W))
    val fastCntROut  = fastCntMem.read(rPCIdx)
    val fastCntWOut  = fastCntMem.read(wPCIdx)
    val fastCntWen   = WireInit(false.B)
    val fastCntWData = WireInit(1.U(2.W))
    when(fastCntWen) { fastCntMem.write(wPCIdx, fastCntWData) }

    val hisAndCnts = Mem(entriesNum, UInt((hisWidth + takeCntNum * 2).W))
    val hAcWOut    = hisAndCnts.read(wPCIdx)
    val hisWOut    = hAcWOut(hAcWOut.getWidth - 1, hAcWOut.getWidth - hisWidth)
    val cntWOut    = Wire(Vec(takeCntNum, UInt(2.W)))
    (0 until takeCntNum).foreach(i => { cntWOut(i) := hAcWOut(i * 2 + 1, i * 2) })
    val hisWData = WireInit(0.U(hisWidth.W))
    val cntWData = WireInit(VecInit.fill(takeCntNum)(1.U(2.W)))
    val hAcWen   = WireInit(false.B)
    val hAcWDate = Cat(hisWData, cntWData.asUInt)
    when(hAcWen) { hisAndCnts.write(wPCIdx, hAcWDate) }

    val (resetState, resetSet)   = (WireInit(false.B), WireInit(0.U))
    val _resetState              = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, entriesNum)
    when(resetFinish) { _resetState := false.B }
    when(resetState) {
      hAcWen     := true.B
      fastCntWen := true.B
      tagsWen    := true.B
      clrWen     := true.B
      wPCIdx     := resetSet
    }
    resetState := _resetState
    resetSet   := _resetSet

    // Read ==============================================================
    asg(readRes(i).take, false.B)
    asg(readRes(i).cnt, 0.U(clrWidth.W))
    when(tagsROut === getTag(readPC)) {
      asg(readRes(i).take, fastCntROut > 1.U)
      asg(readRes(i).cnt, clrROut)
    }
    // Write =============================================================
    val realTake = update.data(i).bits
    when(update.data(i).valid && update.instrOff(i)(1, 0) === i.U) {
      val tagMatch = tagsWOut === getTag(writePC)
      val cntZero  = clrWOut === 0.U
      when(tagMatch) {
        asg(clrWData, calNextCnt(clrWOut, true.B))
        clrWen := true.B

        val nextHis = Cat(hisWOut(hisWidth - 2, 0), realTake)
        require(nextHis.getWidth == hisWidth)
        asg(tagsWData, getTag(writePC))
        tagsWen := true.B

        val wCnt = calNextCnt(fastCntWOut, realTake)
        asg(hisWData, nextHis)
        asg(
          cntWData, {
            val foo = WireInit(cntWOut)
            foo(hisWOut) := wCnt
            foo
          }
        )
        hAcWen := true.B
        asg(fastCntWData, Mux(nextHis === hisWOut, wCnt, cntWOut(nextHis)))
        fastCntWen := true.B
      }.elsewhen(cntZero) {
        asg(tagsWData, getTag(writePC))
        tagsWen := true.B

        asg(fastCntWData, 1.U(2.W))
        fastCntWen := true.B

        asg(cntWData, VecInit.fill(takeCntNum)(1.U(2.W)))
        asg(cntWData(0), Mux(realTake, 2.U(2.W), 0.U(2.W)))
        asg(hisWData, 0.U(hisWidth.W) | realTake)
        hAcWen := true.B
      }.otherwise {
        asg(clrWData, calNextCnt(clrWOut, false.B))
        clrWen := true.B
      }
    }
  })
}

class BasicBPU[T <: Data](val gen: T, val idxWidth: Int = 10, useRegs: Boolean = true) extends MycpuModule {
  val update = IO(new Bundle {
    val tagIdx   = Input(UInt((32 - log2Ceil(IcachLineBytes)).W))
    val instrOff = Input(Vec(4, UInt(instrOffWidth.W)))
    val data     = Flipped(Vec(fetchNum, Valid(gen)))
  })
  val readAddr = List.fill(fetchNum)(IO(Flipped(Valid(UWord))))
  val readRes  = List.fill(fetchNum)(IO(Output(gen)))
  def access(address: UInt, ports: Int) = {
    this.readAddr(ports) := address
    this.readRes(ports)
  }
  val lowWidth    = log2Ceil(fetchNum) + 2 //inst is 4 bytes
  val bpuTagWidth = 32 - idxWidth - lowWidth
  val entriesyNum = math.pow(2, idxWidth).toInt

  // can be change for better design
  def hash(address: UInt) = address(idxWidth + lowWidth - 1, lowWidth)

  def missFunc(entry: T, addr: UInt): T = entry
  def getTag(address: UInt) = {
    val res = address(31, idxWidth + lowWidth)
    require(res.getWidth == bpuTagWidth)
    res
  }
  (0 until fetchNum).foreach(i => {

    val ramWidth = gen.getWidth + bpuTagWidth + 1
    val ram = Module(
      DualPortsSRAM(
        gen         = UInt(ramWidth.W),
        set         = entriesyNum,
        useSRAM     = false,
        shouldReset = true,
        holdRead    = false,
        singlePort  = false,
        writefirst  = true
      )
    )
    // write ========================================
    val updatePC = Cat(update.tagIdx, update.instrOff(i), 0.U(2.W))
    val wen      = update.data(i).valid
    when(wen) { assert(updatePC(lowWidth - 1, 2) === i.U) }
    ram.io.w(wen, Cat(update.data(i).bits.asUInt, getTag(updatePC), true.B), hash(updatePC))
    // read ========================================
    // keep input search index stable
    val bpuIdx = HoldUnless(hash(readAddr(i).bits), readAddr(i).valid)

    val readOut   = ram.io.r(true.B, bpuIdx).resp.data
    val entry     = readOut(ramWidth - 1, bpuTagWidth + 1).asTypeOf(gen)
    val tag       = readOut(bpuTagWidth, 1)
    val validBits = readOut(0)

    require(entry.getWidth == gen.getWidth)
    require(tag.getWidth == bpuTagWidth)

    val lastAddr = RegEnable(readAddr(i).bits, 0.U, readAddr(i).valid)
    when(validBits && (tag === getTag(lastAddr))) {
      readRes(i) := entry
    }.otherwise {
      readRes(i) := missFunc(entry, lastAddr)
    }
  })
}

/**
  * single write port and single read port
  * when write and read to same addr in a cycle
  * read data := write data
  * front update: Branch, J(jmp), JAL(jcall)
  * back  update: JR(jret and jr), JALR(jcall)
  * out should keep out until posedge that in.search.valid is true
  */
class BranchTargetBuffer extends BasicBPU(new BtbOutIO(), 10) {
  override def missFunc(entry: BtbOutIO, addr: UInt): BtbOutIO = {
    val out = Wire(new BtbOutIO)
    out.target   := addr + 8.U
    out.instType := BtbType.non
    out
  }
}

/**
  * only back update Branch
  */
class PatternHistoryTable extends BasicBPU(UInt(2.W), 10) {
  override def missFunc(entry: UInt, addr: UInt): UInt = {
    1.U(2.W)
  }
}

object PatternHistoryTable {
  def calNextCnt(cnt: UInt, take: Bool): UInt = {
    require(cnt.getWidth == 2)
    LookupUInt(
      cnt,
      Seq(
        0.U -> Mux(take, 1.U, 0.U),
        1.U -> Mux(take, 2.U, 0.U),
        2.U -> Mux(take, 3.U, 1.U),
        3.U -> Mux(take, 3.U, 2.U)
      )
    )
  }
}

class BpuUpdateIO extends MycpuBundle {
  val pc  = Output(UWord)
  val btb = Valid(new BtbOutIO)
  val pht = Valid(new PhtOutIO)
}
