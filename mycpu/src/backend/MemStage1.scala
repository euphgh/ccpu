package backend
import bundle._
import config._
import chisel3._
import chisel3.util._

/**
  * TODO: load inst wake up :use addsink?
  *
  * cal 32bit Vaddr here
  * tlb.req:=vaddr
  * take tlb.back.tag to next stage
  *
  * TODO: storeEnq signal
  *     rise storeQEnq signal if the inst is store
  */

class MemStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new ReadOpStageOutIO(kind = fuType.Lsu.id)))
    val out = Decoupled(new MemStage1OutIO)
    val tlb = new Bundle {
      val req  = Output(Word)
      val back = Input(Word)
    }
  })
}
