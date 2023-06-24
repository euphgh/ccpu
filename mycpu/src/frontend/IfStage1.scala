package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import chisel3.experimental.conversions._
import cache._
import utils.asg

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

class ICacheInstIO extends MycpuBundle {
  val op    = CacheOp()
  val taglo = UWord
  val index = UInt(cacheIndexWidth.W)
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
    val in      = Flipped(new PreIfOutIO)
    val out     = Decoupled(new IfStage1OutIO)
    val toPreIf = new IfStage1ToPreIf
    val tlb     = new TLBSearchIO

    val bpuUpdateIn = Flipped(new BpuUpdateIO)
    val icacheInst =
      if (enableCacheInst) Some(Flipped(Valid(new ICacheInstIO)))
      else None
  })
  // stage regs ==========================================
  val fakeCacheInst = Flipped(Valid(new ICacheInstIO))
  fakeCacheInst.valid := false.B
  val usableCacheInst = io.icacheInst.getOrElse(fakeCacheInst)
  val isCacheInst     = usableCacheInst.valid
  val update          = io.in.flush || isCacheInst || io.out.ready
  val pc              = RegEnable(io.in.npc, "hbfc00000".U, update)
  val isDelaySlot     = RegEnable(io.in.isDelaySlot, false.B, update)
  io.out.fire := io.out.ready

  // use wire io.in direct ================================
  // >> cache =============================================
  val icache1 = Module(new CacheStage1())
  icache1.in.valid                         := update
  icache1.in.bits.req.index                := Mux(isCacheInst, usableCacheInst.bits.index, getAddrIdx(io.in.npc))
  icache1.in.bits.req.offset               := getOffset(io.in.npc)
  icache1.in.bits.cacheInst.get.valid      := isCacheInst
  icache1.in.bits.cacheInst.get.bits.op    := usableCacheInst.bits.op
  icache1.in.bits.cacheInst.get.bits.taglo := usableCacheInst.bits.taglo
  io.out.bits.iCache <> icache1.out
  // >> bpu ===============================================
  val PCs = (0 until fetchNum).map(i => Cat(io.in.npc(31, 4), (io.in.npc(3, 2) + i.U), "b00".U))
  // >> >> module ============================================
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  // >> >> >> write =======================================
  btb.update.pc := io.bpuUpdateIn.pc
  btb.update.data <> io.bpuUpdateIn.btb
  pht.update.pc := io.bpuUpdateIn.pc
  pht.update.data <> io.bpuUpdateIn.pht
  // >> >> >> read =========================================
  val btbout = Vec(fetchNum, new BtbOutIO)
  val phtout = Vec(fetchNum, UInt(2.W))
  val bpuout = Vec(fetchNum, new PredictResultBundle)
  (0 until fetchNum).foreach(i => {
    btbout(i)         := btb.access(PCs(i))
    phtout(i)         := pht.access(PCs(i))
    bpuout(i).brType  := btbout(i).instType
    bpuout(i).target  := btbout(i).target
    bpuout(i).counter := phtout
  })
  io.out.bits.predictResult := bpuout
  // >> >> >> Mask and Dest ===============================
  val validBranch = Wire(UInt(fetchNum.W))
  val takeMask    = Wire(UInt(fetchNum.W))
  val dsMask      = Wire(UInt(fetchNum.W)) // the validMask when branch and it's ds are valid
  (0 until fetchNum).foreach(i => {
    val isTakeBr = bpuout(i).counter > 1.U && bpuout(i).brType === BranchType.b
    val isTakeJp = BranchType.isJump(bpuout(i).brType)
    takeMask(i)    := isTakeJp || isTakeBr
    validBranch(i) := takeMask(i) && alignMask(i)
  })
  (io.toPreIf.predictDst, io.toPreIf.dsFetched, dsMask) := PriorityMux(
    Seq(
      validBranch(0) -> (bpuout(0).target, alignMask(1), "b0011".U),
      validBranch(1) -> (bpuout(1).target, alignMask(2), "b0111".U),
      validBranch(2) -> (bpuout(2).target, alignMask(3), "b1111".U),
      validBranch(3) -> (bpuout(3).target, false.B, DontCare)
    )
  )
  io.toPreIf.hasBranch  := validBranch.orR
  io.out.bits.validMask := Mux(io.toPreIf.hasBranch && io.toPreIf.dsFetched, dsMask, alignMask)

  // use regs in, only combinatorial logic ================
  // >> output ================
  val inst4to2  = pc(3, 2)
  val addrError = pc(1, 0).orR
  io.toPreIf.pcVal  := pc
  io.out.bits.pcVal := pc
  import chisel3.util.experimental.decode._
  val alignMask = decoder(
    inst4to2,
    TruthTable(
      Seq(
        BitPat("b?00") -> BitPat("b1111"),
        BitPat("b100") -> BitPat("b1111"),
        BitPat("b101") -> BitPat("b0111"),
        BitPat("b110") -> BitPat("b0011"),
        BitPat("b111") -> BitPat("b0001")
      ),
      BitPat("b1111")
    )
  )
  // >> tlb ================
  io.tlb.req                 := pc
  io.out.bits.tagOfInstGroup := io.tlb.res.pTag
  io.out.bits.exception := MuxCase(
    FrontExcCode.NONE,
    Seq(
      addrError          -> FrontExcCode.AdEL,
      io.tlb.res.noFound -> Mux(io.tlb.res.refill, FrontExcCode.RefillTLBL, FrontExcCode.InvalidTLBL)
    )
  )
  io.out.bits.isUncached := io.tlb.res.ccAttr =/= CCAttr.Cached

  asg(io.toPreIf.stage1Rdy, io.out.ready)
}
