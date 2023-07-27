package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._

class BtbOutIO extends MycpuBundle {
  val instType = BtbType()
  val target   = UWord
}

class BasicBPU[T <: Data](val gen: T, val idxWidth: Int = 10) extends MycpuModule {
  val update = IO(new Bundle {
    val pc   = Input(UWord)
    val data = Flipped(Valid(gen))
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
    val wen = update.data.valid && update.pc(lowWidth - 1, 2) === i.U
    ram.io.w(wen, Cat(update.data.bits.asUInt, getTag(update.pc), true.B), hash(update.pc))
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
    out.target   := addr + 4.U
    out.instType := BtbType.non
    out
  }
}

/**
  * only back update Branch
  */
class PatternHistoryTable extends BasicBPU(UInt(2.W)) {
  override def missFunc(entry: UInt, addr: UInt): UInt = {
    0.U(2.W)
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
