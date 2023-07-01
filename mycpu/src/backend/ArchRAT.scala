package backend

import bundle._
import config._
import chisel3._
import frontend._
import chisel3.util._
import utils.asg
import chisel3.util.experimental.BoringUtils._

class ArchRAT extends MycpuModule {
  val io = IO(new Bundle {
    val retire  = Vec(retireNum, Flipped(Valid(new RATWriteBackIO)))
    val recover = Vec(aRegNum, new SRATEntry)
  })
  val pIdxMap = RegInit(VecInit((0 until aRegNum).map(i => i.U(pRegAddrWidth.W))))
  (0 until aRegNum).foreach(i => {
    val writeMap = (0 until retireNum)
      .map(j => {
        ((io.retire(j).bits.aDest === i.U) && io.retire(j).valid) -> io.retire(j).bits.pDest
      })
      .reverse
    when(VecInit(writeMap.map(_._1)).asUInt.orR) {
      pIdxMap(i) := MuxCase(0.U, writeMap)
    }
    io.recover(i).inPrf := true.B
    io.recover(i).pIdx  := pIdxMap(i)
    (0 until aRegNum).foreach(i => {
      addSink(pIdxMap(i), s"DiffArchRegNum$i")
    })
  })
}
