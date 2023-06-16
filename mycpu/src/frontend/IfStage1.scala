package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._

class BtbOutIO extends MycpuBundle {
  val instType = BranchType()
  val target   = UWord
}

/**
  * single write port and single read port
  * when write and read to same addr in a cycle
  * read data := write data
  *
  * out should keep out until posedge that in.search.valid is true
  */
class BranchTargetBuffer extends MycpuModule {
  val update = IO(new Bundle {
    val pc   = Input(UWord)
    val data = Flipped(Valid(new BtbOutIO))
  })
  val btbIdxWidth = 10
  val btbTagWidth = 32 - btbIdxWidth - 2
  def getBTBTag(address: UInt) = address(31, btbIdxWidth + 2)
  class BTBEntry extends MycpuBundle {
    val valid = Bool()
    val tag   = UInt(btbTagWidth.W)
    val data  = new BtbOutIO()
  }
  val ram = SyncReadMem(math.pow(2, btbIdxWidth).toInt, new BTBEntry)

  // can be change for better design
  def hash(address: UInt) = address(btbIdxWidth + 2 - 1, 2)

  // write ========================================
  when(update.data.valid) {
    val wdata = Wire(new BTBEntry)
    wdata.data  := update.data
    wdata.valid := true.B
    wdata.tag   := getBTBTag(update.pc)
    ram.write(hash(update.pc), wdata)
  }

  // read ========================================
  def access(address: UInt) = {
    val entry = ram.read(hash(address))
    val res   = Wire(new BtbOutIO)
    when(entry.valid && (entry.tag === getBTBTag(address))) {
      res := entry.data
    }.otherwise {
      res.target   := address
      res.instType := BranchType.non
    }
    res
  }

}
class PatternHistoryTable extends MycpuModule {
  val update = IO(new Bundle {
    val pc   = Input(UWord)
    val data = Flipped(Valid(UInt(2.W)))
  })
  val phtIdxWidth = 10
  val phtTagWidth = 32 - phtIdxWidth - 2
  def getphtTag(address: UInt) = address(31, phtIdxWidth + 2)
  class phtEntry extends MycpuBundle {
    val valid = Bool()
    val tag   = UInt(phtTagWidth.W)
    val data  = UInt(2.W)
  }
  val ram = SyncReadMem(math.pow(2, phtIdxWidth).toInt, new phtEntry)

  // can be change for better design
  def hash(address: UInt) = address(phtIdxWidth + 2 - 1, 2)

  // write ========================================
  when(update.data.valid) {
    val wdata = Wire(new phtEntry)
    wdata.valid := true.B
    wdata.tag   := getphtTag(update.pc)
    ram.write(hash(update.pc), wdata)
  }

  // read ========================================
  def access(address: UInt) = {
    val entry = ram.read(hash(address))
    val res   = Wire(UInt(2.W))
    when(entry.valid && (entry.tag === getphtTag(address))) {
      res := entry.data
    }.otherwise {
      res := 0.U
    }
    res
  }
}

class BpuUpdateIO extends MycpuBundle {
  val pc       = Output(UWord)
  val btb      = Valid(new BtbOutIO)
  val pht      = Valid(UInt(2.W))
  val moreData = Output(UInt(1.W)) //TODO:not make sure
}

/**
  * not connect by pipeline
  * out.pcVal = regEnable(preIFOutIO.npc, fire)
  *
  * out.bpuOut give predict result after fire posedge
  * should keep result until next fire posedge
  *
  * out.alignMask is calculate by out.pcVal
  *
  * out.tagOfInstGroup is calculate by out.pcVal
  * connect TLB module by tlb bundle
  * tlb.req = out.pcVal
  * out.tagOfInstGroup  = tlb.back
  *
  * out.exception != NONE when tlb exception or address error
  *
  * instantiate I-cache stage1 in this module
  * all out should not change until next fire posedge
  * when in.flush || IcacheInst.valid = true
  * stage1 must update all out in next cycle
  *
  * instantiate BPU in this module, let out.bpuout = bpu.out
  */
class IfStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(new PreIfOutIO)
    val out = Decoupled(new IfStage1OutIO)
    val tlb = new TLBSearchIO

    val bpuUpdateIn = Flipped(new BpuUpdateIO)
    val delaySlotOK = Output(Bool()) // only to PreIf
    val IcacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new Bundle {
        val op    = CacheOp()
        val taglo = UWord
        val index = UInt(cacheIndexWidth.W)
        // only need index and tag but offset in cache inst
      })))
      else None
  })
  // stage regs ==========================================
  val isCacheInst = io.IcacheInst.get.valid
  val update      = io.in.flush || isCacheInst || io.out.ready
  val pc          = RegEnable(io.in.npc, "hbfc00000".U, update)
  val isDelaySlot = RegEnable(io.in.isDelaySlot, false.B, update)
  io.out.fire := io.out.ready

  // use wire io.in direct ================================
  // >> cache =============================================
  val icache1 = Module(new CacheStage1())
  icache1.in.valid                         := update
  icache1.in.bits.req.index                := Mux(isCacheInst, io.IcacheInst.get.bits.index, getAddrIdx(io.in.npc))
  icache1.in.bits.req.offset               := getOffset(io.in.npc)
  icache1.in.bits.cacheInst.get.valid      := isCacheInst
  icache1.in.bits.cacheInst.get.bits.op    := io.IcacheInst.get.bits.op
  icache1.in.bits.cacheInst.get.bits.taglo := io.IcacheInst.get.bits.taglo
  io.out.bits.iCache <> icache1.out
  // >> bpu ===============================================
  val PCs = (0 to 3).map(i => Cat(io.in.npc(31, 4), (io.in.npc(3, 2) + i.U), "b00".U))
  // >> >> module ============================================
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  // >> >> >> write =======================================
  btb.update.pc := io.bpuUpdateIn.pc
  btb.update.data <> io.bpuUpdateIn.btb
  pht.update.pc := io.bpuUpdateIn.pc
  pht.update.data <> io.bpuUpdateIn.pht
  // >> >> >> read =========================================
  (0 to 3).foreach(i => {
    val btbout = btb.access(PCs(i))
    val phtout = pht.access(PCs(i))
    io.out.bits.predictResult(i).brType  := btbout.instType
    io.out.bits.predictResult(i).target  := btbout.target
    io.out.bits.predictResult(i).counter := phtout
  })

  // use regs in, only combinatorial logic ================
  // >> output ================
  val instL2sb  = pc(3, 2)
  val addrError = pc(1, 0).orR
  io.out.bits.pcVal := pc
  val alignMask = MuxLookup(instL2sb, "b1111".U)(
    Seq(
      "b00".U -> "b1111".U,
      "b01".U -> "b1110".U,
      "b10".U -> "b1100".U,
      "b11".U -> "b1000".U
    )
  )
  io.out.bits.alignMask := Mux(isDelaySlot, "b1000".U, alignMask)
  // >> tlb ================
  io.tlb.req                 := pc
  io.out.bits.tagOfInstGroup := io.tlb.res.pTag
  io.out.bits.exception := MuxCase(
    FrontExcCode.NONE,
    Seq(
      addrError -> FrontExcCode.AdEL,
      io.tlb.res.noFound -> Mux(io.tlb.res.refill, FrontExcCode.RefillTLBL, FrontExcCode.InvalidTLBL)
    )
  )
  io.out.bits.isUncached := io.tlb.res.ccAttr =/= CCAttr.Cached
}
