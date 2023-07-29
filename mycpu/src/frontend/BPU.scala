package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import chisel3.util.experimental.BoringUtils._
import difftest.DifftestSpecRAS
import difftest.DifftestArchRAS

class BtbOutIO extends MycpuBundle {
  val instType = BtbType()
  val target   = UWord
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
  val data     = Vec(fetchNum, Flipped(Valid(UInt(2.W))))
}

class BasicBPU[T <: Data](val gen: T, val idxWidth: Int = 10) extends MycpuModule {
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
    val ram      = Module(new DPTemplate(UInt(ramWidth.W), entriesyNum, true, true))
    // write ========================================
    val updatePC = Cat(update.tagIdx, update.instrOff(i), 0.U(2.W))
    val wen      = update.data(i).valid
    if (verilator) when(wen) { assert(updatePC(lowWidth - 1, 2) === i.U) }
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
class BranchTargetBuffer extends BasicBPU(new BtbOutIO()) {
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
class PatternHistoryTable extends BasicBPU(UInt(2.W)) {
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
  val pht = Valid(UInt(2.W))
}
