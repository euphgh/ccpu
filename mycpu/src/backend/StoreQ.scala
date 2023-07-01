package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg
import cache._
import utils.LookupUInt

class StoreQueue(entries: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val enq    = Flipped(Decoupled(new StoreQIO))
    val retire = Vec(retireNum, Input(Bool()))
    val deq = new Bundle {
      val req  = Decoupled(new StoreQIO)
      val back = Input(Bool())
    }
    val flush = Input(Bool()) // only flush not retire
    val empty = Output(Bool())
    val full  = Output(Bool())
    val query = Flipped(new QuerySQ)
  })

  class StoreQEntry extends MycpuBundle {
    val data    = new StoreQIO
    val valid   = Bool()
    val retired = Bool()
  }
  val ram        = Mem(entries, new StoreQEntry)
  val enq_ptr    = RegInit(0.U(log2Ceil(entries).W))
  val ret_ptr    = RegInit(0.U(log2Ceil(entries).W))
  val deq_ptr    = RegInit(0.U(log2Ceil(entries).W))
  val maybe_full = RegInit(false.B)
  val do_enq     = WireDefault(io.enq.fire)
  val do_deq     = WireDefault(io.deq.back)
  when(do_enq =/= do_deq) {
    maybe_full := do_enq
  }
  val ptr_match = enq_ptr === deq_ptr
  val empty     = ptr_match && !maybe_full
  val full      = ptr_match && maybe_full
  asg(io.full, full)
  asg(io.empty, empty)
  //=================== query ====================
  import chisel3.experimental.conversions._
  (io.query.res.data, io.query.res.sqMask) := MuxCase(
    (0.U(32.W), 0.U(4.W)),
    (0 until entries).map(i => {
      val entryData    = ram(i).data
      val entryLowAddr = entryData.rwReq.lowAddr
      val entryAddr    = Cat(entryData.pTag, entryLowAddr.index, entryLowAddr.offset)
      (entryAddr === io.query.req.addr && ram(i).valid) -> (entryData.rwReq.wWord, entryData.rwReq.wStrb)
    })
  )
  val resMemMask = Wire(Vec(4, Bool()))
  (0 to 3).map(i => {
    resMemMask(i) := io.query.req.needMask(i) && !io.query.res.sqMask(i)
  })
  asg(io.query.res.memMask, resMemMask.asUInt)

  //=================== enq =======================
  io.enq.ready    := !full || io.deq.back
  io.deq.req.bits := ram(deq_ptr).data
  when(do_enq) {
    ram(enq_ptr).data    := io.enq.bits
    ram(enq_ptr).valid   := true.B
    ram(enq_ptr).retired := false.B
    enq_ptr              := enq_ptr + 1.U
  }

  //=================== deq =======================
  val idle :: waitDeq :: Nil = Enum(2)
  val state                  = RegInit(idle)
  io.deq.req.valid := !empty && (state === idle) && (ret_ptr =/= deq_ptr)
  switch(state) {
    is(idle) {
      when(io.deq.req.fire) {
        state := waitDeq
      }
    }
    is(waitDeq) {
      when(io.deq.back) {
        state := idle
      }
    }
  }
  //deq back
  when(do_deq) {
    deq_ptr := deq_ptr + 1.U
  }

  //=================== flush =====================
  //not move deq_ptr, only enq_ptr
  when(io.flush) {
    enq_ptr    := ret_ptr
    maybe_full := false.B
  }

  //=================== retire =====================
  when(io.retire.asUInt.orR) {
    val scommitNum = PopCount(io.retire.asUInt)
    ret_ptr := ret_ptr + scommitNum
    (0 until retireNum).foreach(i => {
      ram(ret_ptr + i.U).retired := i.U < scommitNum
    })
  }
}
