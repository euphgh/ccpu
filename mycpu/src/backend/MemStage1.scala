package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import cache._
import utils._

/**
  * for now,no load inst wake up
  *
  * connect by pipeline
  *   for mem1 in,we set 2 portS for rostage.out and storeQ.out
  *   for mem1 out,we set 1 port for mem2.in(cause storeQ is not in the pipeline)
  *       we only set a storeEnqReq for storeQ.in
  *         connect enq bits in LSU
  *
  * use in.decoded.offset and in.srcData1 to cal 32bit vaddr
  * tlb.req:=vaddr
  * take tlb.back.tag to next stage
  *
  * out.exception != NONE when tlb exception or address error
  *   out.wbRob will change(memReqVaddr exception)
  *
  * instantiate D-cache stage1 in this module
  *   in.cacheIdx/in.cacheoffset/that 4 directly connect to dCache1.in
  *   connect dcache_1.out to MemStage1OutIO.dcache
  *   the "enableCacheInst" is for cacheInst
  */

class MemStage1 extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new MemStage1InIO))
    val out = new Bundle {
      val toStoreQ = Decoupled(new StoreQIO)
      val toMem2   = Decoupled(new MemStage1OutIO)
    }
    val cacheIn = Flipped(Decoupled(new CacheStage1In(true)))
    val tlb     = new TLBSearchIO
  })
  val inBits   = io.in.bits
  val inROplus = io.in.bits.mem1Req.ROplus
  val inSQplus = io.in.bits.mem1Req.SQplus
  //===================== roStage to StoreQ ===========================
  val toStoreQ = io.out.toStoreQ
  val toSQbits = toStoreQ.bits
  val toMem2   = io.out.toMem2
  val toM2Bits = toMem2.bits
  toSQbits.rwReq := inBits.mem1Req.rwReq
  val l2sb       = inBits.mem1Req.rwReq.lowAddr.offset(1, 0)
  val leftSize   = LookupUIntDefault(l2sb, 2.U, Seq(0.U -> 0.U, 1.U -> 1.U))
  val rightSize  = LookupUIntDefault(l2sb, 2.U, Seq(3.U -> 0.U, 2.U -> 1.U))
  val lsSize     = Wire(UInt(2.W))
  val wStrb      = Wire(UInt(4.W))
  val wWord      = Wire(UInt(32.W))
  val byteStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0010".U, 2.U -> "b0100".U, 3.U -> "b1000".U))
  val halfStrob  = LookupUInt(l2sb, Seq(0.U -> "b0011".U, 2.U -> "b1100".U))
  val leftStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0001".U, 2.U -> "b0001".U, 3.U -> "b0011".U))
  val rightStrob = LookupUInt(l2sb, Seq(0.U -> "b1111".U, 1.U -> "b1110".U, 2.U -> "b1100".U, 3.U -> "b1000".U))
  val leftWord   = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0001".U, 2.U -> "b0001".U, 3.U -> "b0011".U))
  val rightWord  = LookupUInt(l2sb, Seq(0.U -> "b1111".U, 1.U -> "b1110".U, 2.U -> "b1100".U, 3.U -> "b1000".U))
  val validBytes = BytesWordUtils.word2Bytes(inBits.mem1Req.rwReq.wWord)
  val swl = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> Cat(validBytes((6 - i) % 4), validBytes((5 - i) % 4), validBytes((4 - i) % 4), validBytes((3 - i) % 4))
    })
    // Seq(
    //   0.U -> Cat(validBytes(2), validBytes(1), validBytes(0), validBytes(3)),
    //   1.U -> Cat(validBytes(1), validBytes(0), validBytes(3), validBytes(2)),
    //   2.U -> Cat(validBytes(0), validBytes(3), validBytes(2), validBytes(1)),
    //   3.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0))
    // )
  )
  val swr = LookupUInt(
    l2sb,
    (0 to 3).map(i => {
      i.U -> Cat(validBytes((7 - i) % 4), validBytes((6 - i) % 4), validBytes((5 - i) % 4), validBytes((4 - i) % 4))
    })
    // Seq(
    //   0.U -> Cat(validBytes(3), validBytes(2), validBytes(1), validBytes(0)),
    //   1.U -> Cat(validBytes(2), validBytes(1), validBytes(0), validBytes(3)),
    //   2.U -> Cat(validBytes(1), validBytes(0), validBytes(3), validBytes(2)),
    //   3.U -> Cat(validBytes(0), validBytes(3), validBytes(2), validBytes(1))
    // )
  )
  import chisel3.experimental.conversions._
  (lsSize, wStrb, wWord) := MuxCase(
    (0.U, 0.U),
    Seq(
      (inBits.memType.asUInt === MemType.bytePat)  -> (0.U, byteStrob, Fill(4, inBits.mem1Req.rwReq.wWord(7, 0))),
      (inBits.memType.asUInt === MemType.halfPat)  -> (1.U, halfStrob, Fill(2, inBits.mem1Req.rwReq.wWord(15, 0))),
      (inBits.memType.asUInt === MemType.wordPat)  -> (2.U, "b1111".U, inBits.mem1Req.rwReq.wWord),
      (inBits.memType.asUInt === MemType.leftPat)  -> (leftSize, leftStrob, swl),
      (inBits.memType.asUInt === MemType.rightPat) -> (rightSize, rightStrob, swr)
    )
  )
  toSQbits.rwReq                                                    := inBits.mem1Req.rwReq
  (toSQbits.rwReq.size, toSQbits.rwReq.wStrb, toSQbits.rwReq.wWord) := (lsSize, wStrb, wWord)
  // >> select ========================================================
  // >> tlb
  val imm    = SignExt(inROplus.immOffset, 32)
  val vTag   = inBits.srcData(0)(31, 22) + imm(31, 22) + inROplus.carryout
  val tlbRes = io.tlb.res
  asg(io.tlb.req, Cat(vTag, 0.U(22.W)))
  toSQbits.pTag  := tlbRes.pTag
  toSQbits.cAttr := tlbRes.ccAttr
  //===================== Exception ===================================
  // only from RoStage has Exception, fromSQ no exception
  val isWriteReq = inBits.mem1Req.rwReq.isWrite
  val tlbExp     = tlbRes.noFound || (isWriteReq && tlbRes.dirty)
  val tlbExcCode = Mux(isWriteReq, Mux(tlbRes.dirty, ExcCode.TLBS, ExcCode.Mod), ExcCode.TLBL)
  val addrErrExp = LookupEnumDefault(inBits.memType, false.B)(
    Seq(
      MemType.SW  -> (l2sb =/= "b00".U),
      MemType.LW  -> (l2sb =/= "b00".U),
      MemType.SH  -> (l2sb(0) =/= "b0".U),
      MemType.LH  -> (l2sb(0) =/= "b0".U),
      MemType.LHU -> (l2sb(0) =/= "b0".U)
    )
  )
  when(tlbExp || addrErrExp) {
    toM2Bits.exception.happen  := true.B
    toM2Bits.exception.excCode := Mux(tlbExp, tlbExcCode, Mux(isWriteReq, ExcCode.AdES, ExcCode.AdEL))
    toM2Bits.exception.refill  := tlbRes.refill
  }
  when(!inBits.isRoStage) {
    toM2Bits.exception.happen := false.B
  }
  val isLaterMem = (isWriteReq || CCAttr.isUnCache(tlbRes.ccAttr.asUInt))
  val lateMemRdy = toStoreQ.ready && toMem2.ready
  // if cache Inst isWriteReq will not set, cacheInst goto mem2
  toStoreQ.valid := io.in.valid && Mux(
    inBits.isRoStage,
    Mux(isLaterMem, lateMemRdy, false.B),
    false.B
  )
  //===================== roStage to Mem2 =============================
  // read from rostage write with exception from rostage, write from SQ should to mem2
  toMem2.valid       := io.in.valid && (Mux(isLaterMem, lateMemRdy, true.B) || !inBits.isRoStage)
  io.in.ready        := Mux(io.in.bits.isRoStage && isWriteReq, toStoreQ.ready, toMem2.ready)
  toM2Bits.isSQ      := !inBits.isRoStage
  toM2Bits.wbInfo    := inBits.wbInfo
  toM2Bits.pTag      := tlbRes.pTag
  toM2Bits.isUncache := CCAttr.isUnCache(tlbRes.ccAttr.asUInt)
  //======================== Cache Stage 1 ============================
  val cache1 = Module(new CacheStage1(DcachRoads, DcachLineBytes, true))
  cache1.in <> io.cacheIn
  toM2Bits.toCache2 <> cache1.out
  when(inBits.isRoStage) {
    toM2Bits.toCache2.dCacheReq.get.size := lsSize
  }
  //TODO: must make sure io.in.ready when cache ready
}
