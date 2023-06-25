package backend
import bundle._
import config._
import chisel3._
import chisel3.util.Valid
import cop._
import utils.asg
import chisel3.util.experimental.BoringUtils
import chisel3.util.Cat

class SimpleWriteBundle extends MycpuBundle {
  val wen   = Output(Bool())
  val wdata = Output(UWord)
}
class Mtc0Bundle extends SimpleWriteBundle {
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
class CP0 extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val eretFlush = Input(Bool())
      val extInt    = Input(UInt(6.W))
      val mfc0Addr  = Input(CP0Idx)
      val mtc0      = Flipped(new Mtc0Bundle)
      val exception = Flipped(Valid(new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      }))
    }
    val out = new Bundle {
      val mfc0Rdata = Output(UWord) // wire logic of mfc0
      val redirect  = new ExceptionRedirectBundle
    }
  })

  val cp0Regs = Module(new COPRegs)

  //mtc0 mfc0
  val c0Waddr = io.in.mtc0.waddr
  val c0Raddr = io.in.mfc0Addr
  when(io.in.mtc0.wen) { cp0Regs.mtc0(sel = c0Waddr(2, 0), rd = c0Waddr(7, 3), data = io.in.mtc0.wdata) }
  asg(io.out.mfc0Rdata, cp0Regs.mfc0(sel = c0Raddr(2, 0), rd = c0Raddr(7, 3)))

  //redirect
  asg(io.out.redirect.valid, io.in.exception.valid || io.in.eretFlush)
  asg(io.out.redirect.nextTarget, Mux(io.in.eretFlush, cp0Regs.epcReg.all, 0xbfc00380.U(32.W))) //FIXME:

  //eret_flush
  when(io.in.eretFlush) { cp0Regs.statusReg.exl := 0.U }

  //exception
  val exInfo = io.in.exception.bits
  when(io.in.exception.valid) {
    cp0Regs.statusReg.exl    := 1.U
    cp0Regs.causeReg.exccode := exInfo.basic.excCode
    when(exInfo.basic.excCode === ExcCode.AdEL || exInfo.basic.excCode === ExcCode.AdES) {
      cp0Regs.badvaddrReg := exInfo.badVaddr
    }
    when(!cp0Regs.statusReg.exl) {
      cp0Regs.causeReg.bd := exInfo.basic.isBd
      cp0Regs.epcReg      := Mux(exInfo.basic.isBd, exInfo.basic.pc - 4.U, exInfo.basic.pc)
    }
  }

  //Interrupt input
  asg(cp0Regs.causeReg.ip_h(5), io.in.extInt(5) | cp0Regs.causeReg.ti)
  asg(cp0Regs.causeReg.ip_h(4, 0), io.in.extInt(4, 0))
  val hasInt =
    ((Cat(cp0Regs.causeReg.ip_h, cp0Regs.causeReg.ip_s) & cp0Regs.statusReg.im) =/= 0.U(8.W)) &&
      cp0Regs.statusReg.ie === 1.U(1.W) &&
      cp0Regs.statusReg.exl === 0.U(1.W)
  BoringUtils.addSource(hasInt, "hasInterrupt")
}
