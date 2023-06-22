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
  * no need decoupled,always rdy
  *
  * after inst retire from rob:
  *     if exception occurred <exception>
  *     if is a eret inst<eretFlush>
  *     if is a mfc0 inst<c0Inst>
  *
  * a mtc0 inst:
  *     can only be dispatched when rob is empty
  *     treat as normal alu inst
  *         but no need to do anything in RO
  *         read c0 in Exe
  */
class CP0 extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {

      val exception = Flipped(Valid(new Bundle {
        val basic    = new ExceptionInfoBundle
        val badVaddr = Output(UWord)
      }))
      val eret = Input(Bool()) //to CP0
      val mtc0 = Flipped(new Mtc0Bundle)

      val mfc0Addr = Input(CP0Idx)
    }
    val out = new Bundle {
      val mfc0Data = Output(UWord) // wire logic of mfc0
      val redirect = new ExceptionRedirectBundle
    }
  })
}
