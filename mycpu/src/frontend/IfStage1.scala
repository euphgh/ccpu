package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._

import cache._
import utils.asg
import chisel3.util.experimental.decode._
import chisel3.util.experimental.BoringUtils

class BtbOutIO extends MycpuBundle {
  val instType = BtbType()
  val target   = UWord
}

class BasicBPU[T <: Data](gen: T, val idxWidth: Int = 10) extends MycpuModule {
  val update = IO(new Bundle {
    val pc   = Input(UWord)
    val data = Flipped(Valid(gen))
  })
  val readAddr = List.fill(fetchNum)(IO(Input(UWord)))
  val readRes  = List.fill(fetchNum)(IO(Output(gen)))
  def access(address: UInt, ports: Int) = {
    this.readAddr(ports) := address
    this.readRes(ports)
  }
  class BPUEntry[T](gen: T) extends MycpuBundle {
    val tag  = UInt(10.W)
    val info = gen
  }
  val lowWidth    = log2Ceil(fetchNum)
  val bpuTagWidth = 32 - idxWidth - lowWidth

  // can be change for better design
  def hash(address:   UInt) = address(idxWidth + lowWidth - 1, lowWidth)
  def missFunc(entry: T, addr: UInt): T = entry
  def getTag(address: UInt) = address(31, idxWidth + 2)
  (0 until fetchNum).foreach(i => {
    val ram   = SyncReadMem(math.pow(2, idxWidth).toInt, new BPUEntry(gen))
    val valid = RegInit(0.U(math.pow(2, idxWidth).toInt.W))
    // write ========================================
    when(update.data.valid && update.pc(lowWidth - 1, 0) === i.U) {
      ram.write(
        hash(update.pc), {
          val wdata = Wire(new BPUEntry(gen))
          wdata.info := update.data.bits
          wdata.tag  := getTag(update.pc)
          wdata
        }
      )
    }
    // read ========================================
    val bpuIdx = hash(readAddr(i))
    val entry  = ram.read(bpuIdx)
    when(valid(bpuIdx) && (entry.tag === getTag(readAddr(i)))) {
      readRes(i) := entry.info
    }.otherwise {
      readRes(i) := missFunc(entry.info, readAddr(i))
    }
  })
}

/**
  * single write port and single read port
  * when write and read to same addr in a cycle
  * read data := write data
  *
  * out should keep out until posedge that in.search.valid is true
  */
class BranchTargetBuffer extends BasicBPU(new BtbOutIO()) {
  override def missFunc(entry: BtbOutIO, addr: UInt): BtbOutIO = {
    val out = new BtbOutIO
    out.target   := addr + 4.U
    out.instType := BtbType.non
    out
  }
}

class PatternHistoryTable extends BasicBPU(UInt(2.W)) {
  override def missFunc(entry: UInt, addr: UInt): UInt = {
    0.U(2.W)
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
  })

  val icacheInst =
    if (enableCacheInst) Some(Wire(Flipped(Valid(new ICacheInstIO))))
    else None
  if (enableCacheInst) {
    BoringUtils.addSink(icacheInst.get, "ICacheInstrReq")
  }

  // stage regs ==========================================
  val fakeCacheInst = Wire(Flipped(Valid(new ICacheInstIO)))
  fakeCacheInst.valid := false.B
  fakeCacheInst.bits  := DontCare
  val usableCacheInst = icacheInst.getOrElse(fakeCacheInst)
  val isCacheInst     = usableCacheInst.valid
  val update          = io.in.flush || isCacheInst || io.out.ready
  val pc              = RegEnable(io.in.npc, "hbfc00000".U, update)
  val isDelaySlot     = RegEnable(io.in.isDelaySlot, false.B, update)

  // use wire io.in direct ================================
  // >> cache =============================================
  val icache1 = Module(new CacheStage1())
  icache1.io.in.valid                         := update
  icache1.io.in.bits.ifReq.get.index          := Mux(isCacheInst, usableCacheInst.bits.index, getAddrIdx(io.in.npc))
  icache1.io.in.bits.ifReq.get.offset         := getOffset(io.in.npc)
  icache1.io.in.bits.cacheInst.get.valid      := isCacheInst
  icache1.io.in.bits.cacheInst.get.bits.op    := usableCacheInst.bits.op
  icache1.io.in.bits.cacheInst.get.bits.taglo := usableCacheInst.bits.taglo
  io.out.bits.iCache <> icache1.io.out
  // >> bpu ===============================================
  val PCs   = (0 until fetchNum).map(i => Cat(io.in.npc(31, 4), (io.in.npc(3, 2) + i.U), "b00".U))
  val bpuPC = (0 until fetchNum).map(i => Cat(io.in.npc(31, 4), i.U(log2Ceil(fetchNum).W), "b00".U))
  // >> >> module ============================================
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  // >> >> >> write =======================================
  btb.update.pc := io.bpuUpdateIn.pc
  btb.update.data <> io.bpuUpdateIn.btb
  pht.update.pc := io.bpuUpdateIn.pc
  pht.update.data <> io.bpuUpdateIn.pht
  // >> >> >> read =========================================
  val bpuout = Wire(Vec(fetchNum, new PredictResultBundle))
  (0 until fetchNum).foreach(i => {
    val alignIdx = i
    val orderIdx = (4.U - i.U)(1, 0)
    val btbRes   = btb.access(Cat(io.in.npc(31, 4), 0.U(4.W)), alignIdx)
    val phtRes   = pht.access(Cat(io.in.npc(31, 4), 0.U(4.W)), alignIdx)
    bpuout(orderIdx).btbType := btbRes.instType
    bpuout(orderIdx).target  := btbRes.target
    bpuout(orderIdx).counter := phtRes
  })
  io.out.bits.predictResult := bpuout
  // >> >> >> Mask and Dest ===============================
  val inst4to2 = pc(4, 2)
  val alignMask = Mux(
    isDelaySlot,
    "b0001".U,
    decoder(
      inst4to2,
      TruthTable(
        Seq(
          BitPat("b0??") -> BitPat("b1111"),
          BitPat("b100") -> BitPat("b1111"),
          BitPat("b101") -> BitPat("b0111"),
          BitPat("b110") -> BitPat("b0011"),
          BitPat("b111") -> BitPat("b0001")
        ),
        BitPat("b1111")
      )
    )
  )
  val validBranch = WireInit(VecInit.fill(fetchNum)(false.B))
  val takeMask    = Wire(Vec(fetchNum, Bool()))
  val dsMask      = Wire(UInt(fetchNum.W)) // the validMask when branch and it's ds are valid
  (0 until fetchNum).foreach(i => {
    val isTakeBr = bpuout(i).counter > 1.U && bpuout(i).btbType === BtbType.b
    val isTakeJp = BtbType.isJump(bpuout(i).btbType)
    takeMask(i)    := isTakeJp || isTakeBr
    validBranch(i) := takeMask(i) && alignMask(i)
  })

  def getByVB[T <: Data](a: Seq[T]) = {
    val res = PriorityMux(
      validBranch.zip(a)
    )
    res
  }
  io.toPreIf.predictDst := getByVB(bpuout.map(_.target))
  io.toPreIf.dsFetched  := getByVB(Seq(alignMask(1), alignMask(2), alignMask(3), false.B))
  dsMask                := getByVB(Seq("b0011".U(4.W), "b0111".U(4.W), "b1111".U(4.W), "b1111".U(4.W)))
  // (io.toPreIf.predictDst, io.toPreIf.dsFetched, dsMask) := PriorityMux(
  //   Seq(
  //     validBranch(0) -> (bpuout(0).target, alignMask(1), "b0011".U(4.W)),
  //     validBranch(1) -> (bpuout(1).target, alignMask(2), "b0111".U(4.W)),
  //     validBranch(2) -> (bpuout(2).target, alignMask(3), "b1111".U(4.W)),
  //     validBranch(3) -> (bpuout(3).target, false.B, "b1111".U(4.W)) //dontcare dsmask
  //   )
  // )
  val dsMaskVec    = Wire(Vec(fetchNum, Bool()))
  val alignMaskVec = Wire(Vec(fetchNum, Bool()))
  (0 until fetchNum).map(i => {
    dsMaskVec(i)    := dsMask(i)
    alignMaskVec(i) := alignMask(i)
  })
  io.toPreIf.hasBranch  := validBranch.asUInt.orR
  io.out.bits.validMask := Mux(io.toPreIf.hasBranch && io.toPreIf.dsFetched, dsMaskVec, alignMaskVec)

  // use regs in, only combinatorial logic ================
  // >> output ================
  val addrError = pc(1, 0).orR
  io.toPreIf.pcVal  := pc
  io.out.bits.pcVal := pc
  // >> tlb ================
  val tlbRes = io.tlb.res
  val tlbExp = tlbRes.refill || !tlbRes.hit
  io.tlb.req.bits            := pc
  io.tlb.req.valid           := true.B
  io.out.bits.tagOfInstGroup := tlbRes.pTag
  io.out.bits.exception := MuxCase(
    FrontExcCode.NONE,
    Seq(
      addrError -> FrontExcCode.AdEL,
      tlbExp -> Mux(
        io.tlb.res.refill,
        FrontExcCode.RefillTLBL,
        FrontExcCode.InvalidTLBL
      )
    )
  )
  io.out.bits.isUncached := io.tlb.res.ccAttr =/= CCAttr.Cached
  if (enableCacheInst) {
    val ci = icacheInst.get
    // index type cache instr should not require tlb
    // becasue, way infomation is in tag
    io.tlb.req.valid := ci.valid && !CacheOp.isHitInv(ci.bits.op)
  }

  asg(io.out.valid, true.B)
  asg(io.toPreIf.stage1Rdy, io.out.fire)
}
