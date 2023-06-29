package cache

import chisel3._
import config._
import bundle.TLBEntry
import bundle._
import chisel3.util._

class TLB extends MycpuModule {
  val search  = IO(Vec(2, new TLBSearchIO))
  val read    = IO(new TLBReadIO)
  val probe   = IO(new TLBProbe)
  val write   = IO(new TLBWriteIO)
  val entries = Reg(VecInit.fill(tlbEntriesNum)(0.U.asTypeOf(new TLBEntry)))

  val asid    = Wire(UInt(8.W))
  val erl     = Bool()
  val k0      = CCAttr()
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
    val vaddr = Mux(dir(i), probe.req.bits, search(i).req.bits)
    hitMask(i) := VecInit((0 until tlbEntriesNum).map(j => {
      val entry   = entries(j)
      val addrHit = entry.vpn2 === vaddr(31, 13)
      (entry.g || (entry.asid === asid)) && addrHit
    }))
    // tlb should not hit 2 entry
    assert(PopCount(hitMask(i)) < 2.U)
    val hitEntry = Mux1H(hitMask(i), entries)
    val isOdd    = vaddr(12)
    tlbRes(i).refill := hitMask(i).asUInt.orR
    // if refill set hit will not valid
    tlbRes(i).hit    := Mux(isOdd, hitEntry.v1, hitEntry.v0)
    tlbRes(i).dirty  := Mux(isOdd, hitEntry.d1, hitEntry.d0)
    tlbRes(i).pTag   := Cat(Mux(isOdd, hitEntry.pfn1, hitEntry.pfn0), (vaddr & "hfff".U(12.W)))
    tlbRes(i).ccAttr := Mux(isOdd, hitEntry.c1, hitEntry.c0)

    // only make sure res.pTag(log2(way number)) not change in index type cache Instr
    search(i).res := Mux(dir(i), dirRes, tlbRes(i))
  })
  read.res := entries(read.req)
  when(write.req.valid) {
    entries(write.req.bits.idx) := write.req.bits.entry
  }
  probe.req.ready := dir(0) || dir(1)
  probe.res.found := Mux(dir(0), hitMask(0).orR, hitMask(1).orR)
  probe.res.index := Mux(dir(0), OHToUInt(hitMask(0)), OHToUInt(hitMask(1)))
}
