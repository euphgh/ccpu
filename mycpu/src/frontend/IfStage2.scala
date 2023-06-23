package frontend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils._
import cache._

/**
  * out.predictResult := stage1.out.bpuOut(bpuOut.takenMask become a "taken" bit)
  *
  * out.basicInstInfo := Instructs from I-Cache
  *
  * out.validNum := func(alignMask, bpuOut.takenMask)
  *
  * out.exception := in.exception
  *
  * pass abort signal to cacheStage2 in this stage
  * if tlb is miss or pc is not aligned
  *
  * pay attention
  * 1. cache must keep data until instBuffer has space
  * 2. ready can not be set from in.iCache.cacheInst.valid to cache redirect
  *
  * when miss,use DramReadIO to connect Dram
  * this connect to IfStage1 can use pipeline connect
  * so all io.in is from regs
  */
class IfStage2 extends Module with MycpuParam {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new IfStage1OutIO))
    val out  = Decoupled(new IfStage2OutIO)
    val imem = new DramReadIO
  })
  val icache2 = Module(new CacheStage2(IcachRoads, IcachLineBytes)())
  icache2.io.in.valid := io.in.valid
  io.in.ready         := icache2.io.in.ready
  asg(icache2.io.in.bits.fromStage1, io.in.bits.iCache)
  asg(icache2.io.in.bits.isException, io.in.bits.exception =/= FrontExcCode.NONE)
  asg(icache2.io.in.bits.isUncached, io.in.bits.isUncached)
  asg(icache2.io.in.bits.ptag, io.in.bits.tagOfInstGroup)
  (0 until fetchNum).foreach(i => {
    io.out.bits.predictResult(i) := io.in.bits.predictResult(i)
    io.out.bits.exception        := io.in.bits.exception
    asg(
      io.out.bits.basicInstInfo(i).pcVal,
      Cat(io.in.bits.pcVal(31, 5), io.in.bits.pcVal(4, 2) + i.U, io.in.bits.pcVal)
    )
    asg(io.out.bits.basicInstInfo(i).instr, icache2.io.out.bits.idata.get(i))
    io.out.bits.validMask(i) := io.in.bits.validMask(i)
  })
  io.out.valid         := icache2.io.out.valid
  icache2.io.out.ready := io.out.ready
}
