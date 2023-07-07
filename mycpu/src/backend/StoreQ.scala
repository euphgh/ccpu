package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg
import cache._
import utils.LookupUInt
import chisel3.util.experimental.BoringUtils
import utils.ZeroExt
import utils.StoreQUtils._

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
    val data = new StoreQIO
  }
  val counterWidth = log2Ceil(entries)
  val ptrWidth     = counterWidth + 1
  val ram          = RegInit(VecInit.fill(entries)(0.U.asTypeOf(new StoreQEntry)))
  val enq_ptr      = RegInit(0.U(ptrWidth.W))
  val ret_ptr      = RegInit(0.U(ptrWidth.W))
  val deq_ptr      = RegInit(0.U(ptrWidth.W))
  val do_enq       = WireDefault(io.enq.fire)
  val do_deq       = WireDefault(io.deq.back)
  val counterMatch = enq_ptr(counterWidth - 1, 0) === deq_ptr(counterWidth - 1, 0)
  val signMatch    = enq_ptr(ptrWidth - 1) === deq_ptr(ptrWidth - 1)
  val empty        = counterMatch && signMatch
  val full         = counterMatch && !signMatch
  asg(io.full, full)
  asg(io.empty, empty)
  //=================== query ====================

  val addrMatch = WireInit(VecInit.fill(entries)(false.B))
  val strbMatch = WireInit(VecInit.fill(4)(VecInit.fill(entries)(false.B))) //4行 entries列
  (0 until entries).map(i => {
    val entryData    = ram(i).data
    val entryLowAddr = entryData.rwReq.lowAddr
    val entryAddr    = Cat(entryData.pTag, entryLowAddr.index, entryLowAddr.offset)
    addrMatch(i) := entryAddr === io.query.req.addr
    (0 until 4).map(j => strbMatch(j)(i) := entryData.rwReq.wStrb(j) & io.query.req.needMask(j)) //j:第几个Byte
  })

  val getStqData = WireInit(VecInit.fill(4)(0.U(8.W)))
  val getSqMask  = WireInit(VecInit.fill(4)(false.B))
  val getMemMask = WireInit(VecInit.fill(4)(false.B))
  (0 until 4).map(i => {
    val matchWen = addrMatch.asUInt & strbMatch(i).asUInt
    when(matchWen.orR) { //没有匹配上的Byte保证全0
      val idx = getByteIndex(enqPtr = enq_ptr, deqPtr = deq_ptr, matchWen = matchWen, entries = entries)
      getStqData(i) := ram(idx).data.rwReq.wWord((i + 1) * 8 - 1, i * 8)
    }
    val queryRes = io.query.res
    asg(getSqMask(i), matchWen.orR)
    asg(getMemMask(i), !getSqMask(i) & io.query.req.needMask(i))
  })
  io.query.res.data    := getStqData.asUInt
  io.query.res.sqMask  := getSqMask.asUInt
  io.query.res.memMask := getMemMask.asUInt

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
    enq_ptr := ret_ptr
  }

  //=================== retire =====================
  when(io.retire.asUInt.orR) {
    val scommitNum = PopCount(io.retire.asUInt)
    ret_ptr := ret_ptr + scommitNum
  }

  // val extInt = Wire(UInt(6.W))
  // BoringUtils.addSink(extInt, "extInt")
  // val allAddr = WireInit(
  //   VecInit(
  //     (0 until entries).map(i =>
  //       Cat(ram(i).data.pTag, ram(i).data.rwReq.lowAddr.index, ram(i).data.rwReq.lowAddr.offset)
  //     )
  //   )
  // )
  // val a = allAddr.asUInt
  // assert((a & ZeroExt(extInt, entries * 32)) === 0.U)
}
