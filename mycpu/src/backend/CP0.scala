package backend
import bundle._
import config._
import chisel3._
import cop._
import utils._
import chisel3.util.experimental.BoringUtils._
import chisel3.util._

class Mtc0Bundle extends MycpuBundle {
  val wen   = Output(Bool())
  val wdata = Output(UWord)
  val waddr = Output(CP0Idx)
}

/**
  * after inst retire from rob:
  *     if exception occurred <exception>
  *     if is a eret inst<eretFlush>
  *     if is a mtc0 <mtc0>(no need "Valid",gen wen in MDU)
  *       use Queue in Mdu to record wdata and coAddr
  *       ROB send an commit signal to MDU,MDU dequeue the related Queue
  *         and send (wen,wdata,c0Addr) to CP0
  * a mfc0 inst:
  *     can only be dispatched when rob is empty
  *     deal in MDU:
  *         do nothing in RO
  *         read c0 in EXE
  *
  * we have make sure that at 1 cycle
  *   at most 1 inst can retire to CP0!
  */
class CP0 extends BasicCOP with MycpuParam {
  val io = IO(new Bundle {
    val in = new Bundle {
      val eretFlush = Input(Bool())
      val extInt    = Input(UInt(6.W))
      val mtc0      = Flipped(new Mtc0Bundle)
      val exCommit  = Flipped(Valid(new ExCommitBundle))
    }
    val mfc0 = new Bundle {
      val addr  = Input(CP0Idx)
      val rdata = Output(UWord)
    }
    val redirectTarget = Output(UWord) //eret exception redirect target
  })
  val exCommit = io.in.exCommit.bits
  val badVaddr = exCommit.badVaddr
  val excCode  = exCommit.detect.excCode
  val llbit    = RegInit(false.B)
  addSource(llbit, "llbit")

  // write llbits
  val llWen = Wire(Bool())
  addSink(llWen, "llWen")
  when(llWen) { llbit := true.B }

  when(io.in.eretFlush) {
    statusReg.exl := 0.U
    llbit         := false.B
  }
  //mtc0 mfc0
  val c0Waddr = io.in.mtc0.waddr
  val c0Raddr = io.mfc0.addr
  when(io.in.mtc0.wen) { mtc0(sel = c0Waddr(2, 0), rd = c0Waddr(7, 3), data = io.in.mtc0.wdata) }
  asg(io.mfc0.rdata, mfc0(sel = c0Raddr(2, 0), rd = c0Raddr(7, 3)))

  //redirect Target
  val trapBase =
    Mux(statusReg.bev === 1.U, "hbfc0_0200".U(32.W), (ebaseReg.eptbase << 12 | "h8000_0000".U(32.W)))
  val trapOffs = WireInit(0x180.U(32.W))
  asg(io.redirectTarget, Mux(io.in.eretFlush, epcReg.all, trapBase + trapOffs))

  //exception
  when(io.in.exCommit.valid) {
    statusReg.exl    := 1.U
    causeReg.exccode := excCode.asUInt
    switch(excCode) {
      is(Seq(ExcCode.TLBL, ExcCode.TLBS, ExcCode.Mod)) {
        contextReg.badvpn2 := exCommit.badVaddr(31, 13)
        entryhiReg.vpn2    := exCommit.badVaddr(31, 13)
        badvaddrReg.all    := exCommit.badVaddr
      }
      is(Seq(ExcCode.AdEL, ExcCode.AdES)) {
        badvaddrReg.all := exCommit.badVaddr
      }
    }
    when(statusReg.exl === 0.U) {
      causeReg.bd := exCommit.basic.isBd
      epcReg.all  := Mux(exCommit.basic.isBd, exCommit.basic.pc - 4.U, exCommit.basic.pc)
      trapOffs := Mux(
        exCommit.detect.refill,
        0.U(32.W),
        (Mux(
          excCode === ExcCode.Int && causeReg.iv === 1.U && statusReg.bev === 1.U,
          0x200.U(32.W),
          0x180.U(32.W)
        ))
      )
    }
  }

  // TLB instr ===========================================
  // >> tlbp  ============================================
  val tlbpWreq  = Wire(Bool())
  val tlbpFound = Wire(Bool())
  val tlbpIndex = Wire(UInt(tlbIndexWidth.W))
  addSink(tlbpWreq, "tlbpRes") // when tlb set tlbpRes to mdu, data ok
  addSink(tlbpFound, "tlbpFound")
  addSink(tlbpIndex, "tlbpIndex")
  when(tlbpWreq) {
    asg(indexReg.p, !tlbpFound)
    asg(indexReg.index, tlbpIndex)
  }
  // >> tlbr  =============================================
  val tlbrReq   = Wire(Bool())
  val tlbrEntry = Wire(new TLBEntry)
  addSink(tlbrReq, "tlbrReq")
  addSink(tlbrEntry, "tlbrEntry")
  when(tlbrReq) {
    asg(entryhiReg.asid, tlbrEntry.asid)
    asg(entryhiReg.vpn2, tlbrEntry.vpn2)
    asg(entrylo0Reg.pfn, tlbrEntry.pfn0)
    asg(entrylo0Reg.c, tlbrEntry.c0)
    asg(entrylo0Reg.d, tlbrEntry.d0)
    asg(entrylo0Reg.v, tlbrEntry.v0)
    asg(entrylo0Reg.g, tlbrEntry.g)
    asg(entrylo1Reg.pfn, tlbrEntry.pfn1)
    asg(entrylo1Reg.c, tlbrEntry.c1)
    asg(entrylo1Reg.d, tlbrEntry.d1)
    asg(entrylo1Reg.v, tlbrEntry.v1)
    asg(entrylo1Reg.g, tlbrEntry.g)
  }

  // addSource to out
  addSource(entryhiReg, "EntryHi")
  addSource(entrylo0Reg, "EntryLo0")
  addSource(entrylo1Reg, "EntryLo1")
  addSource(indexReg, "Index")
  addSource(randomReg, "Random")
  addSource(config0Reg, "config0")
  addSource(statusReg, "status")

  //Interrupt input
  asg(causeReg.iph7, causeReg.iph7.asBool || io.in.extInt(5) || (countReg.all === compareReg.all))
  when(compareWen) {
    causeReg.iph7 := false.B
  }
  asg(causeReg.iph6, io.in.extInt(4))
  asg(causeReg.iph5, io.in.extInt(3))
  asg(causeReg.iph4, io.in.extInt(2))
  asg(causeReg.iph3, io.in.extInt(1))
  asg(causeReg.iph2, io.in.extInt(0))
  val hasInt = Wire(Bool())
  hasInt := ((Cat(
    causeReg.iph7,
    causeReg.iph6,
    causeReg.iph5,
    causeReg.iph4,
    causeReg.iph3,
    causeReg.iph2,
    causeReg.ips1,
    causeReg.ips0
  ) & statusReg.im) =/= 0.U(8.W)) &&
    statusReg.ie === 1.U(1.W) &&
    statusReg.exl === 0.U(1.W)

  addSource(hasInt, "hasInterrupt")

  // DiffTest ===============================================
  import difftest.DifftestArchCP0
  if (verilator) {
    val difftestCP0 = Module(new DifftestArchCP0)
    difftestCP0.io.clock    := clock
    difftestCP0.io.index    := indexReg.read
    difftestCP0.io.random   := randomReg.read
    difftestCP0.io.entrylo0 := entrylo0Reg.read
    difftestCP0.io.entrylo1 := entrylo1Reg.read
    difftestCP0.io.context  := contextReg.read
    difftestCP0.io.pagemask := pagemaskReg.read
    difftestCP0.io.wire     := wireReg.read
    difftestCP0.io.badvaddr := badvaddrReg.read
    difftestCP0.io.count    := countReg.read
    difftestCP0.io.entryhi  := entryhiReg.read
    difftestCP0.io.compare  := compareReg.read
    difftestCP0.io.status   := statusReg.read
    difftestCP0.io.cause    := causeReg.read
    difftestCP0.io.epc      := epcReg.read
    difftestCP0.io.prid     := pridReg.read
    difftestCP0.io.ebase    := ebaseReg.read
    difftestCP0.io.config0  := config0Reg.read
    difftestCP0.io.config1  := config1Reg.read
    // val checkCp0En = RegNext()
    // val checkCp0En = RegNext(io.in.eretFlush || io.in.mtc0.wen || io.in.exCommit.valid || tlbpWreq || tlbrReq)
    // asg(difftestCP0.io.en, checkCp0En)
    addSink(difftestCP0.io.en, "hasValidRetire")
  }

}
