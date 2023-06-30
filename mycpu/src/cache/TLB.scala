package cache

import chisel3._
import config._
import bundle.TLBEntry
import bundle._
import chisel3.util._
import utils._

class TLB extends MycpuModule {

  val search  = IO(Vec(2, Flipped(new TLBSearchIO)))
  val read    = IO(new TLBReadIO)
  val probe   = IO(new TLBProbe)
  val write   = IO(new TLBWriteIO)

  val entries = Reg(VecInit.fill(tlbEntriesNum)(0.U.asTypeOf(new TLBEntry)))
  // aliases =======================================
  import cop._
  import chisel3.util.experimental.BoringUtils._
  // read from cp0
  val entryhiReg  = new entryhi
  val entrylo0Reg = new entrylo0
  val entrylo1Reg = new entrylo1
  val indexReg    = new index
  val randomReg   = new random
  val config0Reg  = new config0
  val statusReg   = new status
  addSink(entryhiReg, "EntryHi")
  addSink(entrylo0Reg, "EntryLo0")
  addSink(entrylo1Reg, "EntryLo1")
  addSink(indexReg, "Index")
  addSink(randomReg, "Random")
  addSink(config0Reg, "config0")
  addSink(statusReg, "status")
  // req from mdu
  val tlbpReq  = Bool()
  val tlbpRes  = Bool()
  val tlbrReq  = Bool()
  val tlbwiReq = Bool()
  val tlbwrReq = Bool()
  addSink(tlbpReq, "tlbpReq")
  addSource(tlbpRes, "tlbpRes")
  addSink(tlbwiReq, "tlbwiReq")
  addSink(tlbwrReq, "tlbwrReq")
  // data to cp0
  val tlbpFound = Bool()
  val tlbpIndex = UInt(6.W)
  addSource(tlbpIndex, "tlbpIndex")
  addSource(tlbpFound, "tlbpFound")
  val tlbrEntry = new TLBEntry
  addSource(tlbrEntry, "tlbrEntry")

  val asid = entryhiReg.asid
  val erl  = Bool()
  asg(erl, statusReg.erl)
  val k0      = config0Reg.k0
  val tlbRes  = List.fill(2)(new TLBSearchRes)
  val hitMask = List.fill(2)(UInt(tlbEntriesNum.W))
  val dir     = List.fill(2)(Bool()) // search is use tlb or not

  (0 until 2).foreach(i => {
    val searchAddr = search(i).req.bits
    dir(i) := ((searchAddr === "b10".U) && erl || !search(i).req.valid)
    // direct result
    val dirRes = new TLBSearchRes
    dirRes.refill := false.B
    dirRes.hit    := true.B
    dirRes.dirty  := false.B
    dirRes.pTag   := searchAddr & "h1fff_ffff".U
    dirRes.ccAttr := Mux(searchAddr(29), CCAttr.Uncached, k0)

    // when not use tlb translate, use tlb for probe
    val reqVpn = Mux(dir(i), indexReg.index, search(i).req.bits(31, 13))
    hitMask(i) := VecInit((0 until tlbEntriesNum).map(j => {
      val entry   = entries(j)
      val addrHit = entry.vpn2 === reqVpn
      (entry.g || (entry.asid === asid)) && addrHit
    }))
    // tlb should not hit 2 entry
    assert(PopCount(hitMask(i)) < 2.U)
    val hitEntry = Mux1H(hitMask(i), entries)
    val isOdd    = searchAddr(i)(12)
    tlbRes(i).refill := hitMask(i).asUInt.orR
    // if refill set hit will not valid
    tlbRes(i).hit    := Mux(isOdd, hitEntry.v1, hitEntry.v0)
    tlbRes(i).dirty  := Mux(isOdd, hitEntry.d1, hitEntry.d0)
    tlbRes(i).pTag   := Cat(Mux(isOdd, hitEntry.pfn1, hitEntry.pfn0), (searchAddr(i) & "hfff".U(12.W)))
    tlbRes(i).ccAttr := Mux(isOdd, hitEntry.c1, hitEntry.c0)

    // only make sure res.pTag(log2(way number)) not change in index type cache Instr
    search(i).res := Mux(dir(i), dirRes, tlbRes(i))
  })
  tlbrEntry := entries(indexReg.index)
  when(tlbwiReq || tlbwrReq) {
    entries(Mux(tlbwiReq, indexReg.index, randomReg.random)) := {
      val wEntry = new TLBEntry
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
  tlbpRes   := dir(0) || dir(1)
  tlbpFound := Mux(dir(0), hitMask(0).orR, hitMask(1).orR)
  tlbpIndex := Mux(dir(0), OHToUInt(hitMask(0)), OHToUInt(hitMask(1)))
}
