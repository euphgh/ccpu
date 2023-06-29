package backend

import bundle._
import config._
import chisel3._
import frontend._
import chisel3.util._

class ArchRAT extends MycpuModule {
  val io = IO(new Bundle {
    val retire  = Vec(retireNum, Valid(new RATWriteBackIO))
    val recover = Vec(aRegNum, new SRATEntry)
  })
  val pIdxMap = RegInit(VecInit((0 until aRegNum).map(i => i.U(pRegAddrWidth.W))))
  (0 to aRegNum).foreach(i => {
    val writeMap = (0 to retireNum)
      .map(j => {
        ((io.retire(i).bits.aDest === i.U) && io.retire(j).valid) -> io.retire(i).bits.pDest
      })
      .reverse
    when(VecInit(writeMap.map(_._1)).asUInt.orR) {
      pIdxMap(i) := MuxCase(0.U, writeMap)
    }
    io.recover(i).inPrf := true.B
    io.recover(i).pIdx  := pIdxMap(i)
  })
}
