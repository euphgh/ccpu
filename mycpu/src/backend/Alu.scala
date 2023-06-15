package backend
import config._
import backend._
import bundle._
import chisel3._

//TODO: bypass ports
class Alu(main: Boolean) extends FuncUnit(FuType.MainAlu) {
  val bypPassIO = IO(new Bundle {
    val in  = Vec(aluBypassNum, Flipped(new WPrfBundle))
    val out = new WPrfBundle
  })
  val extInt = if (main) Some(IO(UInt(6.W))) else None
}
