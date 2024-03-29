package cache

import chisel3._
import config._
import bundle.TLBEntry
import bundle._
import chisel3.util._
import utils._
import difftest.DifftestTLBAll
import config.MycpuInit.TLBReset

class TLB extends MycpuModule {
  val search  = IO(Vec(2, Flipped(new TLBSearchIO)))
  val entries = RegInit(VecInit(TLBReset))
  // aliases =======================================
  import cop._
  import chisel3.util.experimental.BoringUtils._
  // read from cp0
  val entryhiReg  = Wire(new entryhi)
  val entrylo0Reg = Wire(new entrylo0)
  val entrylo1Reg = Wire(new entrylo1)
  val indexReg    = Wire(new index)
  val randomReg   = Wire(new random)
  val config0Reg  = Wire(new config0)
  val statusReg   = Wire(new status)
  addSink(entryhiReg, "EntryHi")
  addSink(entrylo0Reg, "EntryLo0")
  addSink(entrylo1Reg, "EntryLo1")
  addSink(indexReg, "Index")
  addSink(randomReg, "Random")
  addSink(config0Reg, "config0")
  addSink(statusReg, "status")
  // req from mdu
  val tlbpReq  = Wire(Bool())
  val tlbpRes  = Wire(Bool())
  val tlbrReq  = Wire(Bool())
  val tlbwiReq = Wire(Bool())
  val tlbwrReq = Wire(Bool())
  addSink(tlbpReq, "tlbpReq")
  addSink(tlbrReq, "tlbrReq")
  addSource(tlbpRes, "tlbpRes")
  addSink(tlbwiReq, "tlbwiReq")
  addSink(tlbwrReq, "tlbwrReq")
  // data to cp0
  val tlbpFound = Wire(Bool())
  val tlbpIndex = Wire(UInt(6.W))
  addSource(tlbpIndex, "tlbpIndex")
  addSource(tlbpFound, "tlbpFound")
  val tlbrEntry = Wire(new TLBEntry)
  addSource(tlbrEntry, "tlbrEntry")

  val asid = entryhiReg.asid
  val erl  = Wire(Bool())
  asg(erl, statusReg.erl)
  val k0      = config0Reg.k0
  val tlbRes  = List.fill(2)(Wire(new TLBSearchRes))
  val hitMask = List.fill(2)(Wire(UInt(tlbEntriesNum.W)))
  val dir     = List.fill(2)(Wire(Bool())) // search is use tlb or not
  def getTag(address: UInt) = address(31, 31 - tagWidth + 1)

  (0 until 2).foreach(i => {
    val searchAddr = search(i).req.bits
    dir(i) := ((searchAddr(31, 30) === "b10".U) || erl || !search(i).req.valid)
    // direct result
    val dirRes = TLBSearchRes.dir(
      Mux(searchAddr(29), CCAttr.Uncached, CCAttr.safe(k0)._1),
      getTag(searchAddr & "h1fff_ffff".U(32.W))
    )
    // when not use tlb translate, use tlb for probe
    val reqVpn = Mux(dir(i), entryhiReg.vpn2, search(i).req.bits(31, 13))
    hitMask(i) := VecInit((0 until tlbEntriesNum).map(j => {
      val entry   = entries(j)
      val addrHit = entry.vpn2 === reqVpn
      (entry.g || (entry.asid === asid)) && addrHit
    })).asUInt
    val validMask0 = VecInit((0 until tlbEntriesNum).map(entries(_).v0)).asUInt
    val validMask1 = VecInit((0 until tlbEntriesNum).map(entries(_).v1)).asUInt

    // tlb should not hit 2 entry, but can not assert for ld in misPrePath has error addr
    // assert(PopCount(hitMask(i) & validMask0) < 2.U)
    // assert(PopCount(hitMask(i) & validMask1) < 2.U)
    val hitEntry = Mux1H(hitMask(i), entries)
    val isOdd    = searchAddr(12)
    tlbRes(i).refill := hitMask(i).asUInt.orR === false.B
    tlbRes(i).hit    := Mux(isOdd, hitEntry.v1, hitEntry.v0) && !tlbRes(i).refill
    tlbRes(i).dirty  := Mux(isOdd, hitEntry.d1, hitEntry.d0)
    asg(tlbRes(i).pTag, getTag(Cat(Mux(isOdd, hitEntry.pfn1, hitEntry.pfn0), (searchAddr(i) & "hfff".U(12.W)))))
    tlbRes(i).ccAttr := Mux(isOdd, CCAttr.safe(hitEntry.c1)._1, CCAttr.safe(hitEntry.c0)._1)

    // only make sure res.pTag(log2(way number)) not change in index type cache Instr
    search(i).res := Mux(dir(i), dirRes, tlbRes(i))
  })
  tlbrEntry := entries(indexReg.index)
  when(tlbwiReq || tlbwrReq) {
    entries(Mux(tlbwiReq, indexReg.index, randomReg.random)) := {
      val wEntry = Wire(new TLBEntry)
      asg(wEntry.vpn2, entryhiReg.vpn2)
      asg(wEntry.asid, entryhiReg.asid)
      asg(wEntry.g, entrylo0Reg.g | entrylo1Reg.g)
      asg(wEntry.pfn0, entrylo0Reg.pfn)
      asg(wEntry.c0, entrylo0Reg.c)
      asg(wEntry.d0, entrylo0Reg.d)
      asg(wEntry.v0, entrylo0Reg.v)
      asg(wEntry.pfn1, entrylo1Reg.pfn)
      asg(wEntry.c1, entrylo1Reg.c)
      asg(wEntry.d1, entrylo1Reg.d)
      asg(wEntry.v1, entrylo1Reg.v)
      wEntry
    }
  }
  tlbpRes   := (dir(0) || dir(1)) && tlbpReq
  tlbpFound := Mux(dir(0), hitMask(0).orR, hitMask(1).orR)
  tlbpIndex := Mux(dir(0), OHToUInt(hitMask(0)), OHToUInt(hitMask(1)))
  if (verilator) {
    val difftestTLB = Module(new DifftestTLBAll)
    asg(difftestTLB.io.g, VecInit(entries.map(e => e.g)))
    asg(difftestTLB.io.v0, VecInit(entries.map(e => e.v0)))
    asg(difftestTLB.io.v1, VecInit(entries.map(e => e.v1)))
    asg(difftestTLB.io.d0, VecInit(entries.map(e => e.d0)))
    asg(difftestTLB.io.d1, VecInit(entries.map(e => e.d1)))
    asg(difftestTLB.io.c0, VecInit(entries.map(e => e.c0)))
    asg(difftestTLB.io.c1, VecInit(entries.map(e => e.c1)))
    asg(difftestTLB.io.pfn0, VecInit(entries.map(e => e.pfn0)))
    asg(difftestTLB.io.pfn1, VecInit(entries.map(e => e.pfn1)))
    asg(difftestTLB.io.vpn2, VecInit(entries.map(e => e.vpn2)))
    asg(difftestTLB.io.asid, VecInit(entries.map(e => e.asid)))
    difftestTLB.io.clock := clock
    difftestTLB.io.en    := RegNext(tlbwiReq || tlbwrReq, false.B)
    difftestTLB.io.rand  := RegNext(randomReg.random)
    asg(difftestTLB.io.iswr, RegNext(tlbwrReq))
    val mduPC = Wire(UWord)
    addSink(mduPC, "mduPC")
    difftestTLB.io.pc := RegNext(mduPC)
  }
}
