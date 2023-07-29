package backend

import config._
import chisel3._
import chisel3.util._
import utils._
import utils.BytesWordUtils._
import bundle._
import MemType._

class MemStage2OutIO extends MycpuBundle {
  val wbInfo     = new WriteBackIO
  val prevDstSrc = UWord
  val exDetect   = new DetectExInfoBundle
  val memType    = MemType()
  val cacheData  = UWord
  val cacheMask  = UInt(4.W)
  val storeQData = UWord
  val l2sb       = UInt(2.W)
  val isCIntr    = Bool()
  val debugPC    = if (debug) Some(UWord) else None
}

class MemStage3 extends MycpuModule {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new MemStage2OutIO))
    val out = Decoupled(new FunctionUnitOutIO)
  })
  val outBits = io.out.bits
  val inBits  = io.in.bits
  outBits.wbRob.robIndex     := inBits.wbInfo.robIndex
  outBits.wbRob.isMispredict := false.B
  outBits.wbRob.exDetect     := inBits.exDetect
  outBits.wPrf.pDest         := inBits.wbInfo.destPregAddr
  outBits.destAregAddr       := inBits.wbInfo.destAregAddr
  if (debug) asg(outBits.wbRob.debugPC.get, inBits.debugPC.get)
  // alias ======================================================
  val isld       = isLoad(inBits.memType)
  val prevDstSrc = inBits.prevDstSrc
  // ===================== select ===============================
  val cacheMask  = inBits.cacheMask
  val validWord  = maskWord(inBits.cacheData, cacheMask).asUInt | maskWord(inBits.storeQData, ~cacheMask).asUInt
  val validBytes = word2Bytes(validWord)
  val l2sb       = inBits.l2sb
  // >> align ====================================================
  // >> >> bytes =================================================
  val byteRes = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> validBytes(i)
    })
  )
  val lb  = SignExt(byteRes, 32)
  val lbu = ZeroExt(byteRes, 32)
  // >> >> half ====================================================
  val halfRes = Mux(l2sb(1), Cat(validBytes(3), validBytes(2)), Cat(validBytes(1), validBytes(0)))
  val lh      = SignExt(halfRes, 32)
  val lhu     = ZeroExt(halfRes, 32)
  // >> >> word ====================================================
  val lw = validWord
  // >> not align ==================================================
  val lwl = LookupUInt(
    l2sb,
    Seq(
      0.U -> Cat(validBytes(0), prevDstSrc(2), prevDstSrc(1), prevDstSrc(0)),
      1.U -> Cat(validBytes(1), validBytes(0), prevDstSrc(1), prevDstSrc(0)),
      2.U -> Cat(validBytes(2), validBytes(1), validBytes(0), prevDstSrc(0)),
      3.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0))
    )
  )
  val lwr = LookupUInt(
    l2sb,
    Seq(
      0.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0)),
      1.U -> Cat(prevDstSrc(3), validBytes(3), validBytes(2), validBytes(1)),
      2.U -> Cat(prevDstSrc(3), prevDstSrc(2), validBytes(3), validBytes(2)),
      3.U -> Cat(prevDstSrc(3), prevDstSrc(2), prevDstSrc(1), validBytes(3))
    )
  )
  asg(
    io.out.bits.wPrf.result,
    LookupEnum(
      inBits.memType,
      Seq(
        MemType.LB  -> lb,
        MemType.LBU -> lbu,
        MemType.LH  -> lh,
        MemType.LHU -> lhu,
        MemType.LW  -> lw,
        MemType.LL  -> lw,
        MemType.LWL -> lwl,
        MemType.LWR -> lwr
      )
    )
  )
  asg(io.out.bits.wPrf.wmask, Mux(isld, "b1111".U(4.W), 0.U))
  asg(io.out.valid, io.in.valid)
  asg(io.in.ready, io.out.ready)
}
