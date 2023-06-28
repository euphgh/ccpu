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
class CP0 extends BasicCOP with MycpuParam {
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
      val mfc0Rdata      = Output(UWord) //wire logic of mfc0
      val redirectTarget = Output(UWord) //eret exception redirect target
    }
  })
  val exInfo = io.in.exception.bits

  when(io.in.eretFlush) { statusReg.exl := 0.U }
  //mtc0 mfc0
  val c0Waddr = io.in.mtc0.waddr
  val c0Raddr = io.in.mfc0Addr
  when(io.in.mtc0.wen) { mtc0(sel = c0Waddr(2, 0), rd = c0Waddr(7, 3), data = io.in.mtc0.wdata) }
  asg(io.out.mfc0Rdata, mfc0(sel = c0Raddr(2, 0), rd = c0Raddr(7, 3)))

  //redirect Target
  //TODO:remember to select from cache inst redirect target in backend
  val trapBase =
    Mux(statusReg.bev === 1.U, "hbfc0_0200".U(32.W), (ebaseReg.eptbase << 12 | "h8000_0000".U(32.W)))
  val trapOffs = WireInit(0x180.U(32.W))
  asg(io.out.redirectTarget, trapBase + trapOffs)

  //exception
  when(io.in.exception.valid) {
    statusReg.exl    := 1.U
    causeReg.exccode := exInfo.basic.excCode.asUInt
    switch(exInfo.basic.excCode) {
      is(Seq(ExcCode.TLBL, ExcCode.TLBS, ExcCode.Mod)) {
        contextReg.badvpn2 := exInfo.badVaddr(31, 13)
        entryhiReg.vpn2    := exInfo.badVaddr(31, 13)
        badvaddrReg.all    := exInfo.badVaddr
      }
      is(Seq(ExcCode.AdEL, ExcCode.AdES)) {
        badvaddrReg.all := exInfo.badVaddr
      }
    }
    when(statusReg.exl === 0.U) {
      causeReg.bd := exInfo.basic.isBd
      epcReg.all  := Mux(exInfo.basic.isBd, exInfo.basic.pc - 4.U, exInfo.basic.pc)
      trapOffs := Mux(
        exInfo.basic.refill,
        0.U(32.W),
        (Mux(
          exInfo.basic.excCode === ExcCode.Int && causeReg.iv === 1.U && statusReg.bev === 1.U,
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
}
