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
import config.MycpuObject.basicBpuIdxWidth

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
  val writeFirstUpdate = true
  val writeFirstRead   = true

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
  class MemFirstW[T <: Data](
    size:      BigInt,
    t:         T,
    readPorts: Seq[Boolean] = Seq(false))
      extends MycpuModule {
    val readPortsNum = readPorts.length
    val idxWidth     = log2Ceil(size)
    val readReq      = List.fill(readPortsNum)(IO(Input(UInt(idxWidth.W))))
    val readRes      = List.fill(readPortsNum)(IO(Output(t)))
    val wen          = IO(Input(Bool()))
    val wData        = IO(Input(t))
    val wAddr        = IO(Input(UInt(idxWidth.W)))
    val ram          = Mem(size, t)
    (0 until readPortsNum).foreach(i => {
      readRes(i) := ram.read(readReq(i))
    })
    when(wen) {
      ram.write(wAddr, wData)
      (0 until readPortsNum).foreach(i => {
        if (readPorts(i)) {
          when(readReq(i) === wAddr) { readRes(i) := wData }
        }
      })
    }
    def access(num: Int, idx: UInt): T = {
      require(num >= 0)
      require(num < readPortsNum)
      readReq(num) := idx
      readRes(num)
    }
    def write(enable: Bool, index: UInt, data: T) = {
      wen   := enable
      wAddr := index
      wData := data
    }
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
  val writePCr = Reg(UWord); writePCr := writePC

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
    val rPCIdx  = getIdx(readPC)
    val wPCIdx  = WireInit(getIdx(writePC))
    val wPCIdxR = WireInit(getIdx(writePCr))
    // write when valid
    val clrMem   = Module(new MemFirstW(entriesNum, UInt(clrWidth.W), Seq(writeFirstRead, writeFirstUpdate)))
    val clrROut  = clrMem.access(0, rPCIdx)
    val clrWOut  = clrMem.access(1, wPCIdx)
    val clrWen   = WireInit(false.B)
    val clrWData = WireInit(0.U(clrWidth.W))
    clrMem.write(clrWen, wPCIdxR, clrWData)

    val tagsMem   = Module(new MemFirstW(entriesNum, UInt(lhtTagWidth.W), Seq(writeFirstRead, writeFirstUpdate)))
    val tagsROut  = tagsMem.access(0, rPCIdx)
    val tagsWOut  = tagsMem.access(1, wPCIdx)
    val tagsWen   = WireInit(false.B)
    val tagsWData = WireInit(0.U(lhtTagWidth.W))
    tagsMem.write(tagsWen, wPCIdxR, tagsWData)

    val fastCntMem   = Module(new MemFirstW(entriesNum, UInt(2.W), Seq(writeFirstRead, writeFirstUpdate)))
    val fastCntROut  = fastCntMem.access(0, rPCIdx)
    val fastCntWOut  = fastCntMem.access(1, wPCIdx)
    val fastCntWen   = WireInit(false.B)
    val fastCntWData = WireInit(1.U(2.W))
    fastCntMem.write(fastCntWen, wPCIdxR, fastCntWData)

    val hisAndCnts = Module(new MemFirstW(entriesNum, UInt((hisWidth + takeCntNum * 2).W), Seq(writeFirstUpdate)))
    val hAcWOut    = hisAndCnts.access(0, wPCIdx)
    val hisWOut    = hAcWOut(hAcWOut.getWidth - 1, hAcWOut.getWidth - hisWidth)
    val cntWOut    = Wire(Vec(takeCntNum, UInt(2.W)))
    (0 until takeCntNum).foreach(i => { cntWOut(i) := hAcWOut(i * 2 + 1, i * 2) })
    val hisWData = WireInit(0.U(hisWidth.W))
    val cntWData = WireInit(VecInit.fill(takeCntNum)(1.U(2.W)))
    val hAcWen   = WireInit(false.B)
    val hAcWDate = Cat(hisWData, cntWData.asUInt)
    hisAndCnts.write(hAcWen, wPCIdxR, hAcWDate)

    val (resetState, resetSet)   = (WireInit(false.B), WireInit(0.U))
    val _resetState              = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, entriesNum)
    when(resetFinish) { _resetState := false.B }
    when(resetState) {
      hAcWen     := true.B
      fastCntWen := true.B
      tagsWen    := true.B
      clrWen     := true.B
      wPCIdxR    := resetSet
    }
    resetState := _resetState
    resetSet   := _resetSet

    // Read ==============================================================
    asg(readRes(i).take, false.B)
    asg(readRes(i).cnt, 0.U(clrWidth.W))
    val tagHit = Wire(Bool())
    asg(tagHit, tagsROut === getTag(readPC))
    when(tagHit) {
      asg(readRes(i).take, fastCntROut > 1.U)
      asg(readRes(i).cnt, clrROut)
    }
    // Write =============================================================
    val realTake  = update.data(i).bits
    val realTakeR = RegNext(realTake)

    val write2 = RegNext(update.data(i).valid && update.instrOff(i)(1, 0) === i.U)

    val nextHisW = Cat(hisWOut(hisWidth - 2, 0), realTake)
    val wCntW    = calNextCnt(fastCntWOut, realTake)

    dontTouch(writePC)
    val tagMatchR       = RegNext(tagsWOut === getTag(writePC))
    val cntZeroR        = RegNext(clrWOut === 0.U)
    val clrWDataPlus1R  = RegNext(calNextCnt(clrWOut, true.B))
    val clrWDataMinus1R = RegNext(calNextCnt(clrWOut, false.B))
    val nextHisR        = RegNext(nextHisW)
    val wCntR           = RegNext(wCntW)
    val nextTakeCntsR = RegNext {
      val newCntData = WireInit(cntWOut)
      newCntData(hisWOut) := wCntW
      newCntData
    }
    val nextFastCntR = RegNext(Mux(nextHisW === hisWOut, wCntW, cntWOut(nextHisW)))
    when(write2) {
      when(tagMatchR) {
        clrWData := clrWDataPlus1R
        clrWen   := true.B

        asg(tagsWData, getTag(writePCr))
        tagsWen := true.B

        hisWData := nextHisR
        cntWData := nextTakeCntsR
        hAcWen   := true.B

        fastCntWData := nextFastCntR
        fastCntWen   := true.B
      }.elsewhen(cntZeroR) {
        tagsWData := getTag(writePCr)
        tagsWen   := true.B

        asg(fastCntWData, Mux(realTakeR, 1.U(2.W), 0.U(2.W)))
        fastCntWen := true.B

        asg(cntWData, VecInit.fill(takeCntNum)(1.U(2.W)))
        asg(cntWData(0), Mux(realTakeR, 2.U(2.W), 0.U(2.W)))
        asg(hisWData, 0.U(hisWidth.W) | realTakeR)
        hAcWen := true.B
      }.otherwise {
        clrWData := clrWDataMinus1R
        clrWen   := true.B
      }
    }
  })
}

class BasicBPU[T <: Data](val gen: T, val idxWidth: Int = basicBpuIdxWidth, useRegs: Boolean = true)
    extends MycpuModule {
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
    require(address.getWidth == 32)
    val res = address(31, idxWidth + lowWidth)
    require(res.getWidth == bpuTagWidth)
    res
  }
  (0 until fetchNum).foreach(i => {

    val ramWidth = gen.getWidth + bpuTagWidth
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
    ram.io.w(wen, Cat(update.data(i).bits.asUInt, getTag(updatePC)), hash(updatePC))
    // read ========================================
    val readOut = ram.io.r(readAddr(i).valid, hash(readAddr(i).bits)).resp.data
    val entry   = readOut(ramWidth - 1, bpuTagWidth).asTypeOf(gen)

    val tagHit = Wire(Bool())
    val tag    = Wire(UInt(bpuTagWidth.W))

    require(entry.getWidth == gen.getWidth)
    require(tag.getWidth == bpuTagWidth)

    val lastAddr = RegEnable(readAddr(i).bits, readAddr(i).valid)
    asg(tag, readOut(bpuTagWidth - 1, 0))
    asg(tagHit, tag === getTag(RegEnable(readAddr(i).bits, readAddr(i).valid)))
    when(tagHit) {
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
class BranchTargetBuffer extends BasicBPU(new BtbOutIO(), basicBpuIdxWidth) {
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
class PatternHistoryTable extends BasicBPU(UInt(2.W), basicBpuIdxWidth) {
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

object BCache {
  // configurable:
  val idxWidth   = 7
  val memUseSRAM = false
  import MycpuObject.fetchNum
  class BCacheWIO extends MycpuBundle {
    val pc  = UWord
    val dst = UWord
  }
  val lowWidth       = log2Ceil(fetchNum)
  val bCacheTagWidth = 32 - idxWidth - lowWidth
  val entriesyNum    = math.pow(2, idxWidth).toInt
  def cleanMask      = Cat(Fill(bCacheTagWidth, false.B), Fill(lowWidth + idxWidth, true.B))
  // can be change for better design
  def hash(address: UInt) = address(idxWidth + lowWidth - 1, lowWidth)
  def getTag(address: UInt) = {
    require(address.getWidth == 32)
    val res = address(31, idxWidth + lowWidth)
    require(res.getWidth == bCacheTagWidth)
    res
  }
}

class BCache extends MycpuModule {
  val io = IO(new Bundle {
    val readAddr = Flipped(Valid(UWord))
    val readRes  = Valid(UWord)
    val write    = Flipped(Valid(new BCache.BCacheWIO))
  })
  import BCache._
  val ramWidth = bCacheTagWidth + 32
  val ram = Module(
    DualPortsSRAM(
      gen         = UInt(ramWidth.W),
      set         = entriesyNum,
      useSRAM     = memUseSRAM,
      shouldReset = true,
      holdRead    = false,
      singlePort  = false,
      writefirst  = true
    )
  )
  val res  = ram.io.r(io.readAddr.valid, hash(io.readAddr.bits))
  val rTag = res.resp.data(32 + bCacheTagWidth - 1, 32)
  val rDst = res.resp.data(31, 0)
  asg(io.readRes.valid, rTag === getTag(RegEnable(io.readAddr.bits, io.readAddr.valid)))
  asg(io.readRes.bits, rDst)

  val wPC   = io.write.bits.pc
  val wDst  = io.write.bits.dst
  val wData = Wire(UInt(ramWidth.W))
  asg(wData, Cat(getTag(wPC), wDst))
  ram.io.w(io.write.valid, wData, hash(wPC))
}
