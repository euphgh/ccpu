package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

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
      val exception = new Bundle {
        val happen   = Input(Bool())
        val isBd     = Input(Bool())
        val excCode  = Input(ExcCode())
        val badVaddr = Input(UWord)
        val badPc    = Input(UWord)
      }
      val eretFlush = Input(Bool())
      val extIntIn  = Input(UInt(6.W)) //硬件中断输入引脚
      val c0Inst = new Bundle {
        val wen   = Input(Bool()) //mfc0
        val wdata = Input(UWord) //mfc0
        val addr  = Input(UInt(8.W)) //mfc0、mtc0
      }
    }
    val out = new Bundle {
      val c0InstRdata = Output(UWord)
      val c0Epc       = Output(UWord) //for eret_flush inst
      val isAsyInt    = Output(Bool()) //是否发生硬件中断
    }
  })
}
