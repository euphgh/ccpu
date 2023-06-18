package backend
import bundle._
import config._
import chisel3._
import chisel3.util.Valid

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
      val fromRob = new Bundle {
        val exception = Flipped(new Bundle {
          val basic    = new ExceptionInfoBundle
          val badVaddr = Output(UWord)
        })
        val eret = Input(Bool()) //to CP0
        val mtc0 = Flipped(Valid(new Mtc0Bundle)) //to CP0
      }
      val mfc0 = Input(CP0Idx)
    }
    val out = new Bundle {
      val mfc0Data = Output(UWord) // wire logic of mfc0
      val redirect = new ExceptionRedirectBundle
    }
  })
}
