package backend.mem
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg
import cache._
import chisel3.util.experimental.BoringUtils._
import utils.StoreQUtils._
import utils._
import frontend.RATWriteBackIO
import backend.mem.{Mem1ToStqIO, QuerySQ, StoreQIO}

class StoreQueue(entries: Int) extends MycpuModule {
  val io = IO(new Bundle {
    val fromMem1  = Flipped(Decoupled(new Mem1ToStqIO)) //mem1 ro store
    val writeBack = Decoupled(new FunctionUnitOutIO) //writeback
    val wSrat     = Valid(new RATWriteBackIO)

    val retire   = Vec(retireNum, Input(Bool())) //from rob commit
    val retirePC = if (debug) Some(Input(Vec(retireNum, UWord))) else None
    val deq = new Bundle {
      val req    = Decoupled(new StoreQIO) //to mem1
      val back   = Input(Bool()) //from mem2
      val backPC = if (debug) Some(Input(UWord)) else None //from mem2
    }

    val ldFire = Input(Bool()) // saying load fire in mem2

    val query = Flipped(new QuerySQ) //mem2 load query
    val flush = Input(Bool()) // only flush not retired
    val empty = Output(Bool())
    val full  = Output(Bool())
  })

  //=================== alias =======================
  val fromMem1   = io.fromMem1
  val mem1InBits = fromMem1.bits
  val stqEnq     = mem1InBits.stqEnq
  val inExInfo   = mem1InBits.exDetect
  val inWbInfo   = mem1InBits.wbInfo
  val queryBits  = io.query.req

  //=================== pipe reg =======================
  val fromM1Decp = Wire(Decoupled(new Mem1ToStqIO))
  PipelineConnect(fromMem1, fromM1Decp, io.writeBack.fire, io.flush)
  fromM1Decp.ready := fromMem1.ready

  //=================== queue =======================
  val counterWidth = log2Ceil(entries)
  val ptrWidth     = counterWidth + 1
  val ram          = RegInit(VecInit.fill(entries)(0.U.asTypeOf(new StoreQIO)))
  val enq_ptr      = RegInit(0.U(ptrWidth.W))
  val ret_ptr      = RegInit(0.U(ptrWidth.W))
  val req_ptr      = RegInit(0.U(ptrWidth.W)) // deqReq
  val deq_ptr      = RegInit(0.U(ptrWidth.W))
  val do_enq       = WireDefault(io.fromMem1.fire)
  val do_deq       = WireDefault(io.deq.back)
  val counterMatch = enq_ptr(counterWidth - 1, 0) === deq_ptr(counterWidth - 1, 0)
  val signMatch    = enq_ptr(ptrWidth - 1) === deq_ptr(ptrWidth - 1)
  val empty        = counterMatch && signMatch
  val full         = counterMatch && !signMatch
  asg(io.full, full)
  asg(io.empty, empty)

  //=================== enq =======================
  fromMem1.ready := !full && io.writeBack.ready
  when(do_enq && !io.fromMem1.bits.scFail) {
    ram(enq_ptr) := stqEnq
    enq_ptr      := enq_ptr + 1.U
  }

  //=================== WB =======================
  val wb       = io.writeBack
  val wbBits   = wb.bits
  val wPrf     = wbBits.wPrf
  val wRob     = wbBits.wbRob
  val m1DecpWb = fromM1Decp.bits.wbInfo
  val m1DecpEx = fromM1Decp.bits.exDetect
  wb.valid            := fromM1Decp.valid //already pipeline connect,valid means pipex_valid
  wbBits.destAregAddr := m1DecpWb.destAregAddr
  wPrf.pDest          := m1DecpWb.destPregAddr //should be 0
  wPrf.result         := !fromM1Decp.bits.scFail
  wPrf.wmask          := "hf".U(4.W)
  wRob.exDetect       := m1DecpEx
  wRob.isMispredict   := false.B
  wRob.robIndex       := m1DecpWb.robIndex

  io.wSrat.bits.aDest := m1DecpWb.destAregAddr
  io.wSrat.bits.pDest := m1DecpWb.destPregAddr
  io.wSrat.valid      := io.writeBack.fire && fromM1Decp.bits.isSC

  if (debug) wRob.debugPC.get := fromM1Decp.bits.stqEnq.debugPC.get
  //=================== retire =====================
  when(io.retire.asUInt.orR) {
    val scommitNum = PopCount(io.retire.asUInt)
    ret_ptr := ret_ptr + scommitNum
    if (debug) {
      (0 until retireNum).foreach(i => {
        when(io.retire(i)) {
          assert(ram(ret_ptr + OHToUInt(CountMask.oneHot(io.retire.asUInt(i, 0)))).debugPC.get === io.retirePC.get(i))
        }
      })
    }
  }

  //=================== deq =======================
  io.deq.req.valid := !empty && (ret_ptr =/= req_ptr)
  io.deq.req.bits  := ram(req_ptr)
  when(io.deq.req.fire) {
    req_ptr := req_ptr + 1.U
  }
  //deq back
  when(do_deq) {
    deq_ptr := deq_ptr + 1.U
    if (debug) assert(io.deq.backPC.get === ram(deq_ptr).debugPC.get)
  }

  //=================== query ====================
  val queryHead = RegEnable(enq_ptr, io.ldFire)
  assert(Mux(io.ldFire, io.fromMem1.valid === false.B, true.B))
  val addrMatch = WireInit(VecInit.fill(entries)(false.B))
  val strbMatch = WireInit(VecInit.fill(4)(VecInit.fill(entries)(false.B))) //4行 entries列
  (0 until entries).map(i => {
    val entryData    = ram(i)
    val entryLowAddr = entryData.rwReq.lowAddr
    val entryAddr    = Cat(entryData.pTag, entryLowAddr.index, entryLowAddr.offset)
    addrMatch(i) := entryAddr(31, 2) === io.query.req.addr(31, 2)
    (0 until 4).map(j => strbMatch(j)(i) := entryData.rwReq.wStrb(j) & io.query.req.needMask(j)) //j:第几个Byte
  })
  val getStqData = WireInit(VecInit.fill(4)(0.U(8.W)))
  val getSqMask  = WireInit(VecInit.fill(4)(false.B))
  val getMemMask = WireInit(VecInit.fill(4)(false.B))
  (0 until 4).map(i => {
    val matchWen = addrMatch.asUInt & strbMatch(i).asUInt
    val oneHots  = getOHIndex(queryHead, deq_ptr, matchWen, entries)
    asg(getStqData(i), Mux1H(oneHots, (0 until entries).map(ram(_).rwReq.wWord((i + 1) * 8 - 1, i * 8))))
    asg(getSqMask(i), oneHots.orR)

    asg(getMemMask(i), !getSqMask(i) & io.query.req.needMask(i))
  })
  io.query.res.data    := getStqData.asUInt
  io.query.res.sqMask  := getSqMask.asUInt
  io.query.res.memMask := getMemMask.asUInt

  //=================== flush =====================
  when(io.flush) {
    enq_ptr := ret_ptr
  }
}
