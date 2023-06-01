package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * connect rs.out.renamed.srcPregs to prf.readPorts.addr in Backend
  * the readData connect to FU
  *
  * connect FU.wprf to here in Backend
  *     the wb width is 3,now we dicide to block mdu_out if other 3 all produce
  */

class Prf extends MycpuModule {
  val io = IO(new Bundle {
    val readPorts = Vec(
      prfReadPortNum,
      new Bundle {
        val addr  = Input(UInt(pRegAddrWidth.W))
        val datas = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      }
    )
    val writePorts = Vec(wBNum, Flipped(Decoupled(new WPrfBundle)))
  })
}
