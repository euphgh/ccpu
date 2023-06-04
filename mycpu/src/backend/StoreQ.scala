package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

class StoreQueue extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new IfStage2OutIO))
    val out = Vec(decodeNum, Decoupled(new InstBufferOutIO))
  })
}
