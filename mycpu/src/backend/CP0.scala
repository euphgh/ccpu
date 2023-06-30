package backend
import bundle._
import config._
import chisel3._
import chisel3.util.Valid
import cop._
import utils.asg
import chisel3.util.experimental.BoringUtils
import chisel3.util.Cat
import chisel3.util.switch
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

  when(io.in.eretFlush) { statusReg.exl := 0.U }
  //mtc0 mfc0
  val c0Waddr = io.in.mtc0.waddr
  val c0Raddr = io.mfc0.addr
  when(io.in.mtc0.wen) { mtc0(sel = c0Waddr(2, 0), rd = c0Waddr(7, 3), data = io.in.mtc0.wdata) }
  asg(io.mfc0.rdata, mfc0(sel = c0Raddr(2, 0), rd = c0Raddr(7, 3)))

  //redirect Target
  //TODO:remember to select from cache inst redirect target in backend
  val trapBase =
    Mux(statusReg.bev === 1.U, "hbfc0_0200".U(32.W), (ebaseReg.eptbase << 12 | "h8000_0000".U(32.W)))
  val trapOffs = WireInit(0x180.U(32.W))
  asg(io.redirectTarget, trapBase + trapOffs)

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

  //Interrupt input
  asg(causeReg.iph7, io.in.extInt(5) || causeReg.ti.asBool)
  asg(causeReg.iph6, io.in.extInt(4))
  asg(causeReg.iph5, io.in.extInt(3))
  asg(causeReg.iph4, io.in.extInt(2))
  asg(causeReg.iph3, io.in.extInt(1))
  asg(causeReg.iph2, io.in.extInt(0))
  val hasInt =
    ((Cat(
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
  BoringUtils.addSource(hasInt, "hasInterrupt")

  // DiffTest ===============================================
  import difftest.DifftestArchCP0
  if (verilator) {
    val checkCP0Regs = Module(new DifftestArchCP0)
    checkCP0Regs.io.index    := indexReg.read
    checkCP0Regs.io.random   := randomReg.read
    checkCP0Regs.io.entrylo0 := entrylo0Reg.read
    checkCP0Regs.io.entrylo1 := entrylo1Reg.read
    checkCP0Regs.io.context  := contextReg.read
    checkCP0Regs.io.pagemask := pagemaskReg.read
    checkCP0Regs.io.wire     := wireReg.read
    checkCP0Regs.io.badvaddr := badvaddrReg.read
    checkCP0Regs.io.count    := countReg.read
    checkCP0Regs.io.entryhi  := entryhiReg.read
    checkCP0Regs.io.compare  := compareReg.read
    checkCP0Regs.io.status   := statusReg.read
    checkCP0Regs.io.cause    := causeReg.read
    checkCP0Regs.io.epc      := epcReg.read
    checkCP0Regs.io.prid     := pridReg.read
    checkCP0Regs.io.ebase    := ebaseReg.read
    checkCP0Regs.io.config0  := config0Reg.read
    checkCP0Regs.io.config1  := config1Reg.read
    BoringUtils.addSink(checkCP0Regs.io.en, "hasValidRetire")
  }
}
