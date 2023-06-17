import chisel3._
import chisel3.experimental.conversions._

/**
  * Compute GCD using subtraction method.
  * Subtracts the smaller from the larger until register y is zero.
  * value in register x is then the GCD
  */
class GCD extends Module {
  val io = IO(new Bundle {
    val value1        = Input(UInt(16.W))
    val value2        = Input(UInt(16.W))
    val loadingValues = Input(Bool())
    val outputGCD     = Output(UInt(16.W))
    val outputValid   = Output(Bool())
  })

  val x = Reg(UInt())
  val y = Reg(UInt())

  when(x > y) { x := x - y }.otherwise { y := y - x }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD   := x
  io.outputValid := y === 0.U
}

class MyModule extends Module {
  val a, b, c, d = IO(Input(UInt(8.W)))
  val sel        = IO(Input(Bool()))
  val y, z       = IO(Output(UInt(8.W)))
  (y, z) := Mux(sel, (a, b), (c, d))
}
