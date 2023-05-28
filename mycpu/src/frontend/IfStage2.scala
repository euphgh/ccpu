package frontend
import bundle._
import config._
import chisel3._
import chisel3.util.Decoupled

/**
  * out.predictResult := stage1.out.bpuOut
  *
  * out.basicInstInfo := Instructs from I-Cache
  *
  * out.validNum := func(alignMask, bpuOut.takenMask)
  *
  * out.exception :=
  *
  * pass abort signal to cacheStage2 in this stage
  * if tlb is miss or pc is not aligned
  *
  * pay attention: cahce must keep data until instBuffer has space
  */
class IfStage2 extends MycpuModule {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new IfStage1OutIO))
    val out  = Decoupled(new IfStage2OutIO)
    val imem = new DramReadIO
  })
}
