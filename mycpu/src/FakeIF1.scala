package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._

import cache._
import utils.asg
import chisel3.util.experimental.decode._
import chisel3.util.experimental.BoringUtils

class FakeIF1 extends MycpuModule {
  val io = IO(new Bundle {
    val in      = Flipped(new PreIfOutIO)
    val out     = Decoupled(new IfStage1OutIO)
    val toPreIf = new IfStage1ToPreIf
    val tlb     = new TLBSearchIO

    val bpuUpdateIn = Flipped(new BpuUpdateIO)
  })

  // alias ===============================================
  val npc = io.in.npc

  // stage regs ==========================================
  val update      = io.in.flush || io.out.ready
  val pc          = RegEnable(npc, "hbfc00000".U, update)
  val isDelaySlot = RegEnable(io.in.isDelaySlot, false.B, update)

  // use wire io.in direct ================================
  io.out.bits.iCache := 0.U.asTypeOf(new CacheStage1OutIO(4, 7, false))

  // >> bpu ===============================================
  val PCs   = (0 until fetchNum).map(i => Cat(npc(31, 4), (npc(3, 2) + i.U), "b00".U))
  val bpuPC = (0 until fetchNum).map(i => Cat(npc(31, 4), i.U(log2Ceil(fetchNum).W), "b00".U))
  // >> >> module ============================================
  val btb = Module(new BranchTargetBuffer())
  val pht = Module(new PatternHistoryTable())
  // >> >> >> write ======================================
  btb.update.pc := io.bpuUpdateIn.pc
  btb.update.data <> io.bpuUpdateIn.btb
  pht.update.pc := io.bpuUpdateIn.pc
  pht.update.data <> io.bpuUpdateIn.pht
  // >> >> >> read =========================================
  val bpuout = Wire(Vec(fetchNum, new PredictResultBundle))
  val btbRes = Wire(Vec(fetchNum, new BtbOutIO))
  val phtRes = Wire(Vec(fetchNum, UInt(2.W)))
  (0 until fetchNum).foreach(i => {
    btb.readAddr(i) := Cat(npc(31, 4), 0.U(4.W))
    pht.readAddr(i) := Cat(npc(31, 4), 0.U(4.W))
    btbRes(i)       := btb.readRes(i)
    phtRes(i)       := pht.readRes(i)
  })
  (0 until fetchNum).foreach(i => {
    bpuout(i).btbType := btbRes(npc(3, 2)).instType
    bpuout(i).target  := btbRes(npc(3, 2)).target
    bpuout(i).counter := phtRes(npc(3, 2))
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
  // io.toPreIf.predictDst := 0.U
  io.toPreIf.dsFetched := getByVB(Seq(alignMask(1), alignMask(2), alignMask(3), false.B))
  // io.toPreIf.dsFetched := false.B
  dsMask := getByVB(Seq("b0011".U(4.W), "b0111".U(4.W), "b1111".U(4.W), "b1111".U(4.W)))
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

  asg(io.out.valid, true.B)
  asg(io.toPreIf.stage1Rdy, io.out.fire)
}
