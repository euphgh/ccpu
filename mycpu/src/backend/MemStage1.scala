package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import cache._
import utils._
import chisel3.util.experimental.BoringUtils._

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

    val robOldestIdx = Input(ROBIdx) //for uncached load
    val stqEmpty     = Input(Bool()) //for uncached load
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
  val lowAddr    = inBits.mem1Req.rwReq.lowAddr
  val l2sb       = lowAddr.offset(1, 0)
  val leftSize   = LookupUIntDefault(l2sb, 2.U, Seq(0.U -> 0.U, 1.U -> 1.U))
  val rightSize  = LookupUIntDefault(l2sb, 2.U, Seq(3.U -> 0.U, 2.U -> 1.U))
  val byteStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0010".U, 2.U -> "b0100".U, 3.U -> "b1000".U))
  val halfStrob  = LookupUInt(l2sb, Seq(0.U -> "b0011".U, 2.U -> "b1100".U))
  val leftStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0011".U, 2.U -> "b0111".U, 3.U -> "b1111".U))
  val rightStrob = LookupUInt(l2sb, Seq(0.U -> "b1111".U, 1.U -> "b1110".U, 2.U -> "b1100".U, 3.U -> "b1000".U))
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

  def selectByMemType[T <: Data](datas: Seq[T], default: T) = {
    require(datas.length == 5)
    MuxCase(
      default,
      Seq(
        (inBits.memType.asUInt === MemType.bytePat),
        (inBits.memType.asUInt === MemType.halfPat),
        (inBits.memType.asUInt === MemType.wordPat),
        (inBits.memType.asUInt === MemType.leftPat),
        (inBits.memType.asUInt === MemType.rightPat)
      ).zip(datas)
    )
  }
  toSQbits.rwReq.size := selectByMemType(
    Seq(
      0.U(2.W),
      1.U(2.W),
      2.U(2.W),
      leftSize,
      rightSize
    ),
    0.U
  )
  toSQbits.rwReq.wStrb := selectByMemType(
    Seq(
      byteStrob,
      halfStrob,
      "b1111".U(4.W),
      leftStrob,
      rightStrob
    ),
    0.U
  )
  toSQbits.rwReq.wWord := BytesWordUtils
    .maskWord(
      selectByMemType(
        Seq(
          Fill(4, inBits.mem1Req.rwReq.wWord(7, 0)),
          Fill(2, inBits.mem1Req.rwReq.wWord(15, 0)),
          inBits.mem1Req.rwReq.wWord,
          swl,
          swr
        ),
        0.U
      ),
      toSQbits.rwReq.wStrb
    )
    .asUInt
  toSQbits.rwReq := inBits.mem1Req.rwReq

  // >> select ========================================================
  // >> tlb
  val imm    = SignExt(inROplus.immOffset, 32)
  val vTag   = inBits.srcData(0)(31, 22) + imm(31, 22) + inROplus.carryout
  val tlbRes = io.tlb.res
  asg(io.tlb.req.bits, Cat(vTag, 0.U(22.W)))
  io.tlb.req.valid := io.in.valid
  toSQbits.pTag    := tlbRes.pTag
  toSQbits.cAttr   := tlbRes.ccAttr
  //===================== Exception ===================================
  // only from RoStage has Exception, fromSQ no exception
  val isWriteReq = inBits.mem1Req.rwReq.isWrite
  val tlbExp     = (tlbRes.refill || !tlbRes.hit) || (isWriteReq && tlbRes.dirty)
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
  val badAddr = Wire(Valid(UWord))
  badAddr.valid := (tlbExp || addrErrExp) && io.in.valid && !inBits.exDetect.happen
  badAddr.bits  := Cat(vTag, lowAddr.index, lowAddr.offset)
  addSource(badAddr, "mem1BadAddr")
  val inEx   = inBits.exDetect
  val toM2Ex = toM2Bits.exDetect
  asg(toM2Ex.happen, inEx.happen || tlbExp || addrErrExp)
  asg(toM2Ex.refill, inEx.refill || (tlbExp && tlbRes.refill))
  asg(
    toM2Ex.excCode,
    MuxCase(
      ExcCode.AdEL, //dontcare,no exception happen
      Seq(
        inEx.happen -> inEx.excCode,
        addrErrExp  -> Mux(isWriteReq, ExcCode.AdES, ExcCode.AdEL),
        tlbExp      -> tlbExcCode
      )
    )
  )
  when(!inBits.isRoStage) {
    toM2Bits.exDetect.happen := false.B
  }

  val lateMemRdy = toStoreQ.ready && toMem2.ready
  // if cache Inst isWriteReq will not set, cacheInst goto mem2
  toStoreQ.valid := io.in.valid && Mux(
    inBits.isRoStage,
    Mux(isWriteReq, lateMemRdy, false.B),
    false.B
  )
  //===================== roStage to Mem2 =============================
  // read from rostage write with exception from rostage, write from SQ should to mem2
  val blkUcLoad = !isWriteReq && CCAttr.isUnCache(
    tlbRes.ccAttr.asUInt
  ) && !(io.in.bits.wbInfo.robIndex === io.robOldestIdx && io.stqEmpty)
  toMem2.valid       := io.in.valid && (Mux(isWriteReq, lateMemRdy, !blkUcLoad) || !inBits.isRoStage)
  io.in.ready        := Mux(io.in.bits.isRoStage && isWriteReq, lateMemRdy, toMem2.ready)
  toM2Bits.isSQ      := !inBits.isRoStage
  toM2Bits.wbInfo    := inBits.wbInfo
  toM2Bits.pTag      := tlbRes.pTag
  toM2Bits.isUncache := CCAttr.isUnCache(tlbRes.ccAttr.asUInt)
  //======================== Cache Stage 1 ============================
  val cache1 = Module(new CacheStage1(DcachRoads, DcachLineBytes, true))
  cache1.io.in.bits <> io.cacheIn.bits
  cache1.io.in.valid := io.cacheIn.valid && io.in.ready
  toM2Bits.toCache2 <> cache1.io.out
  when(inBits.isRoStage) {
    toM2Bits.toCache2.dCacheReq.get.size := toSQbits.rwReq.size
  }
  // do not important for lsu will not see this signal
  io.cacheIn.ready := cache1.io.in.ready

  if (enableCacheInst) {
    val ci = cache1.io.out.cacheInst.get
    // index type cache instr should not require tlb
    // becasue, way infomation is in tag
    io.tlb.req.valid := ci.valid && !CacheOp.isHitInv(ci.bits.op)
    import frontend.ICacheInstIO
    val toICache1 = Wire(Valid(new ICacheInstIO))
    addSource(toICache1, "ICacheInstrReq")
    toICache1.bits.index := cache1.io.in.bits.rwReq.get.lowAddr.index
    toICache1.bits.taglo := cache1.io.in.bits.cacheInst.get.bits.taglo
    toICache1.bits.op    := cache1.io.in.bits.cacheInst.get.bits.op
    toICache1.valid      := cache1.io.in.valid && io.cacheIn.bits.cacheInst.get.valid
  }

}
