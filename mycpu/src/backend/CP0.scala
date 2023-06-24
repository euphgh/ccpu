package backend
import bundle._
import config._
import chisel3._
import chisel3.util.Valid

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
  */
class CP0 extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val mfc0Addr  = Input(CP0Idx)
      val mtc0      = Flipped(new Mtc0Bundle)
      val eretFlush = Input(Bool())
      val exception = Flipped(Valid(new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      }))
    }
    val out = new Bundle {
      val mfc0Data = Output(UWord) // wire logic of mfc0
      val redirect = new ExceptionRedirectBundle
    }
  })
}
