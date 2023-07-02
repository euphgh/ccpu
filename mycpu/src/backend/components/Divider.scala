package backend.components

import config._
import backend._
import chisel3._
import chisel3.util._
import utils._
class Divider extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
    }))
    val out = Valid(UInt(63.W))
  })

  io.out := DontCare //TODO:
}
