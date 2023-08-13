package backend.mem
import bundle._
import config._
import chisel3._
import chisel3.util._
import cache._
import utils._
import chisel3.util.experimental.BoringUtils._
import chisel3.util.experimental.BoringUtils
import backend.mem.{Mem1ToStqIO, MemStage1InIO, MemStage1OutIO, StoreQIO, WriteBackIO}
import frontend.LocHisTab

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

class StoreQInIO extends MycpuBundle {
  val pTag     = UInt(tagWidth.W) //get in mem1
  val cAttr    = CCAttr()
  val rwReq    = new CacheRWReq(DcachLineBytes)
  val wbInfo   = new WriteBackIO
  val exDetect = new DetectExInfoBundle
}

class MemStage1 extends MycpuModule {
  val io = IO(new Bundle {
    // no pipeline, should save by self
    val fromSQ = Flipped(Decoupled(new StoreQIO))
    val fromRO = Flipped(Decoupled(new MemStage1InIO))
    val out = new Bundle {
      val toStoreQ = Decoupled(new Mem1ToStqIO)
      val toMem2   = Decoupled(new MemStage1OutIO)
    }
    val tlb      = new TLBSearchIO
    val stqEmpty = Input(Bool()) //for uncached load
    val flush    = Input(Bool())

    val oldestRobIdx = Input(ROBIdx)
  })

  val toStoreQ = io.out.toStoreQ
  val toSQbits = toStoreQ.bits
  val toMem2   = io.out.toMem2
  val toM2Bits = toMem2.bits
  // ==================== origin lsu select ==========================

  val wireRo = io.fromRO
  val wireSq = io.fromSQ
  import MemType._
  val wireRoIsLoad = !wireRo.bits.memType.isOneOf(SW, SB, SH, SWL, SWR, SC)
  val nextIsLoad   = wireRo.valid && wireRoIsLoad
  val sqDecp       = Wire(Decoupled(new StoreQIO))
  val roDecp       = Wire(Decoupled(new MemStage1InIO))
  val roFireOut    = Wire(Bool())
  val sqFireOut    = Wire(Bool())
  val sqBits       = sqDecp.bits
  val roBits       = roDecp.bits
  val reCacheOK    = RegInit(false.B)
  val tlbFinish    = RegInit(false.B)
  val realLAddr    = roBits.rLowAddr
  val predictLAddr = roBits.rwReq.lowAddr
  val imm          = SignExt(roBits.immOffset, 32)
  val vTag         = roBits.srcData(0)(31, 12) + imm(31, 12) + roBits.carryOut
  val vaddr        = Cat(vTag, realLAddr.index, realLAddr.offset)
  val dirPaddr     = vaddr & "h1fff_ffff".U
  val dirPtag      = dirPaddr(31, 12)
  val isDir        = roBits.isDir
  val isDirC       = isDir && !CCAttr.isUnCache(roBits.dirCattr.asUInt)
  val isDirUc      = isDir && CCAttr.isUnCache(roBits.dirCattr.asUInt)
  val dirTlbRes    = TLBSearchRes.dir(roBits.dirCattr, dirPtag)
  val idxMiss      = predictLAddr.index =/= realLAddr.index
  val tlbStable    = isDir || tlbFinish
  val idxStable    = !idxMiss || reCacheOK
  val reCacheRW    = Wire(new CacheRWReq(DcachLineBytes))
  reCacheRW                := roDecp.bits.rwReq
  reCacheRW.lowAddr.index  := vaddr(DcacheIndexWidth + DcacheOffsetWidth - 1, DcacheOffsetWidth)
  reCacheRW.lowAddr.offset := vaddr(DcacheOffsetWidth - 1, 0)

  // Mem Index Predict ==============================================
  val mipWIO = Wire(Flipped(Valid(new IndexPredictor.WriteIO)))
  val useMIP = roBits.mipOut.valid && roBits.mipOut.bits.cnt > 1.U
  val cntAdd = (useMIP && !idxMiss) || (!useMIP && idxMiss)
  BoringUtils.addSource(mipWIO, "MIP_WRITE_IO")
  asg(mipWIO.valid, RegNext(io.fromRO.fire && !io.flush))
  asg(mipWIO.bits.pc, roBits.pcVal)
  asg(mipWIO.bits.wData.idx, realLAddr.index)
  asg(mipWIO.bits.wData.cnt, roBits.mipOut.bits.cnt)
  asg(mipWIO.bits.idxMatch, cntAdd)
  asg(mipWIO.bits.tagMatch, roBits.mipOut.valid)

  // TLB =============================================================
  val tlbRes = Mux(isDir, dirTlbRes, RegNext(io.tlb.res))
  io.tlb.req.bits       := RegNext(Cat(vTag, 0.U(12.W)))
  io.tlb.req.valid      := roDecp.valid
  toSQbits.stqEnq.pTag  := tlbRes.pTag
  toSQbits.stqEnq.cAttr := tlbRes.ccAttr
  //===================== Exception ===================================
  // only from RoStage has Exception, fromSQ no exception
  val l2sb       = realLAddr.offset(1, 0)
  val isWriteReq = roBits.rwReq.isWrite
  val tlbExp     = Mux(tlbRes.refill, true.B, Mux(!tlbRes.hit, true.B, Mux(isWriteReq, !tlbRes.dirty, false.B)))
  val tlbExcCode = Mux(isWriteReq, Mux(tlbRes.hit && !tlbRes.dirty, ExcCode.Mod, ExcCode.TLBS), ExcCode.TLBL)
  val addrErrExp = LookupEnumDefault(roBits.memType, false.B)(
    Seq(
      MemType.SW  -> (l2sb =/= "b00".U),
      MemType.SC  -> (l2sb =/= "b00".U),
      MemType.LW  -> (l2sb =/= "b00".U),
      MemType.LL  -> (l2sb =/= "b00".U),
      MemType.SH  -> (l2sb(0) =/= "b0".U),
      MemType.LH  -> (l2sb(0) =/= "b0".U),
      MemType.LHU -> (l2sb(0) =/= "b0".U)
    )
  )
  val badAddr = Wire(Valid(UWord))
  badAddr.valid := (tlbExp || addrErrExp) && roDecp.valid && !roBits.exDetect.happen && tlbStable
  badAddr.bits  := vaddr
  addSource(badAddr, "mem1BadAddr")
  val inEx  = roBits.exDetect
  val outEx = Wire(new DetectExInfoBundle)
  asg(io.out.toMem2.bits.exDetect, outEx)
  asg(io.out.toStoreQ.bits.exDetect, outEx)
  asg(outEx.happen, inEx.happen || tlbExp || addrErrExp)
  asg(outEx.refill, inEx.refill || (tlbExp && tlbRes.refill))
  asg(
    outEx.excCode,
    MuxCase(
      ExcCode.AdEL, //dontcare,no exception happen
      Seq(
        inEx.happen -> inEx.excCode,
        addrErrExp  -> Mux(isWriteReq, ExcCode.AdES, ExcCode.AdEL),
        tlbExp      -> tlbExcCode
      )
    )
  )
  // state ===========================================================
  val storeMode :: cloadMode :: ucloadMode :: cInstrWaitNext :: cInstrGo :: Nil = Enum(5)

  val state     = RegInit(storeMode)
  val cInstrExc = RegNext(outEx.happen)
  val toCache2  = io.out.toMem2.bits.toCache2

  val useSQ :: useRO :: useSelf :: Nil = Enum(3)
  val cache1Update = Wire(new Bundle {
    val req   = Bool()
    val which = UInt(2.W)
  })
  sqDecp.ready       := false.B
  roDecp.ready       := false.B
  cache1Update.which := useRO
  cache1Update.req   := false.B
  toStoreQ.valid     := false.B
  toMem2.valid       := false.B
  roFireOut          := false.B
  sqFireOut          := false.B

  if (debug) toStoreQ.bits.stqEnq.debugPC.get := roBits.debugPC.get
  val sqDecpRdy    = !sqDecp.valid || toMem2.fire
  val roDecpRdy    = !roDecp.valid || toStoreQ.fire
  val storeModeRdy = sqDecpRdy && roDecpRdy
  when(roDecp.valid) { reCacheOK := true.B }
  when(reCacheOK) { tlbFinish := true.B }
  switch(state) {
    is(storeMode) {
      when(nextIsLoad) {
        // prev
        when(storeModeRdy) {
          roDecp.ready       := true.B
          sqDecp.ready       := false.B
          cache1Update.which := useRO
          cache1Update.req   := true.B
          state              := cloadMode
          roFireOut          := toStoreQ.fire
          sqFireOut          := toMem2.fire
        }.otherwise {
          roDecp.ready       := false.B
          sqDecp.ready       := toMem2.ready || !sqDecp.valid
          cache1Update.which := useSQ
          cache1Update.req   := wireSq.fire
          roFireOut          := toStoreQ.fire
          sqFireOut          := toMem2.fire
        }
      }.otherwise {
        // prev
        roDecp.ready       := roDecpRdy
        sqDecp.ready       := toMem2.ready || !sqDecp.valid
        cache1Update.which := useSQ
        cache1Update.req   := wireSq.fire
        roFireOut          := toStoreQ.fire
        sqFireOut          := toMem2.fire
      }
      // next
      toMem2.valid := sqDecp.valid
      val toC2Bits = io.out.toMem2.bits.toCache2
      asg(toC2Bits.dCacheReq.get, sqBits.rwReq)
      if (enableCacheInst) toC2Bits.cacheInst.get.valid := false.B

      toStoreQ.valid := roDecp.valid && tlbStable
    }
    is(cloadMode) { // only one cycle for any load req
      assert(roDecp.valid)
      val isUncache = CCAttr.isUnCache(tlbRes.ccAttr.asUInt) && MemType.isLoad(roBits.memType)
      val isCinstr  = if (enableCacheInst) roDecp.bits.cacheInst.get.valid else false.B
      when(!idxStable || !tlbStable) {
        // prev
        roDecp.ready       := false.B
        sqDecp.ready       := false.B
        cache1Update.which := useSelf
        cache1Update.req   := !idxStable
        roFireOut          := false.B
        sqFireOut          := false.B
        // next
        toMem2.valid   := false.B
        toStoreQ.valid := false.B
      }.elsewhen(isCinstr) {
        state              := cInstrWaitNext
        toMem2.valid       := false.B
        toStoreQ.valid     := false.B
        sqDecp.ready       := false.B
        roDecp.ready       := false.B
        roFireOut          := false.B // must can not
        sqFireOut          := false.B
        cache1Update.which := false.B
        cache1Update.req   := false.B
      }.elsewhen(!isUncache) {
        // next
        toMem2.valid   := true.B
        toStoreQ.valid := false.B
        roFireOut      := toMem2.fire
        sqFireOut      := false.B // for not sq valid
        // prev
        when(nextIsLoad) {
          // state not change
          cache1Update.req   := wireRo.fire
          cache1Update.which := useRO
          roDecp.ready       := toMem2.ready
          sqDecp.ready       := false.B
        }.otherwise {
          cache1Update.req   := wireSq.fire
          cache1Update.which := useSQ
          roDecp.ready       := toMem2.ready
          sqDecp.ready       := toMem2.ready
          when(toMem2.fire) {
            state := storeMode
          }
        }
      }.otherwise {
        toMem2.valid       := false.B
        toStoreQ.valid     := false.B
        sqDecp.ready       := true.B
        roDecp.ready       := false.B
        roFireOut          := false.B // must can not
        sqFireOut          := false.B
        cache1Update.which := useSQ
        cache1Update.req   := wireSq.fire
        state              := ucloadMode
      }
    }
    is(ucloadMode) {
      assert(roDecp.valid && idxStable)
      val wbRobIdx = toMem2.bits.wbInfo.robIndex
      val isOldest = io.oldestRobIdx === wbRobIdx
      when(io.stqEmpty && isOldest) {
        // prev
        roDecp.ready := toMem2.ready
        sqDecp.ready := toMem2.ready
        // next
        toMem2.valid   := true.B
        toStoreQ.valid := false.B
        roFireOut      := toMem2.fire
        sqFireOut      := false.B
        when(toMem2.fire) {
          state              := Mux(nextIsLoad, cloadMode, storeMode)
          cache1Update.which := Mux(nextIsLoad, useRO, useSQ)
          cache1Update.req   := true.B
        }
      }.otherwise {
        // prev
        cache1Update.which := useSQ
        cache1Update.req   := wireSq.fire
        // next
        toMem2.valid   := sqDecp.valid
        toStoreQ.valid := false.B
        roDecp.ready   := false.B
        sqDecp.ready   := toMem2.ready
        roFireOut      := false.B //must can not
        sqFireOut      := toMem2.fire
      }
    }
    is(cInstrWaitNext) {
      state              := cInstrWaitNext
      toMem2.valid       := false.B
      toStoreQ.valid     := false.B
      sqDecp.ready       := false.B
      roDecp.ready       := false.B
      roFireOut          := false.B
      sqFireOut          := false.B
      cache1Update.which := DontCare
      cache1Update.req   := false.B
      val robHead = Wire(ROBIdx)
      addSink(robHead, "ROB_HEAD_PTR")
      when(roBits.wbInfo.robIndex + 1.U =/= robHead && toMem2.ready) {
        state := cInstrGo
      }
    }
    is(cInstrGo) {
      assert(toMem2.ready)
      // next
      toMem2.valid   := true.B
      toStoreQ.valid := false.B
      roFireOut      := true.B
      sqFireOut      := false.B // for not sq valid
      // prev
      when(nextIsLoad) {
        // state not change
        cache1Update.req   := wireRo.fire
        cache1Update.which := useRO
        roDecp.ready       := toMem2.ready
        sqDecp.ready       := false.B
        state              := cloadMode
      }.otherwise {
        cache1Update.req   := wireSq.fire
        cache1Update.which := useSQ
        roDecp.ready       := toMem2.ready
        sqDecp.ready       := toMem2.ready
        state              := storeMode
      }
    }
  }
  when(io.flush) {
    state := storeMode
  }
  //===================== roStage to StoreQ ===========================

  val leftSize   = LookupUIntDefault(l2sb, 2.U, Seq(0.U -> 0.U, 1.U -> 1.U))
  val rightSize  = LookupUIntDefault(l2sb, 2.U, Seq(3.U -> 0.U, 2.U -> 1.U))
  val byteStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0010".U, 2.U -> "b0100".U, 3.U -> "b1000".U))
  val halfStrob  = LookupUInt(l2sb, Seq(0.U -> "b0011".U, 2.U -> "b1100".U))
  val leftStrob  = LookupUInt(l2sb, Seq(0.U -> "b0001".U, 1.U -> "b0011".U, 2.U -> "b0111".U, 3.U -> "b1111".U))
  val rightStrob = LookupUInt(l2sb, Seq(0.U -> "b1111".U, 1.U -> "b1110".U, 2.U -> "b1100".U, 3.U -> "b1000".U))
  val validBytes = BytesWordUtils.word2Bytes(roBits.rwReq.wWord)
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
        (roBits.memType.asUInt === MemType.bytePat),
        (roBits.memType.asUInt === MemType.halfPat),
        (roBits.memType.asUInt === MemType.wordPat),
        (roBits.memType.asUInt === MemType.leftPat),
        (roBits.memType.asUInt === MemType.rightPat)
      ).zip(datas)
    )
  }
  val sizeOfmemType = selectByMemType(
    Seq(
      0.U(3.W),
      1.U(3.W),
      2.U(3.W),
      leftSize,
      rightSize
    ),
    0.U
  )
  val strbOfmemType = selectByMemType(
    Seq(
      byteStrob,
      halfStrob,
      "b1111".U(4.W),
      leftStrob,
      rightStrob
    ),
    0.U
  )

  // default
  toSQbits.wbInfo       := roBits.wbInfo
  toSQbits.stqEnq.rwReq := reCacheRW
  // update
  toSQbits.stqEnq.rwReq.size  := sizeOfmemType
  toSQbits.stqEnq.rwReq.wStrb := strbOfmemType
  toSQbits.stqEnq.rwReq.wWord := BytesWordUtils
    .maskWord(
      selectByMemType(
        Seq(
          Fill(4, roBits.rwReq.wWord(7, 0)),
          Fill(2, roBits.rwReq.wWord(15, 0)),
          roBits.rwReq.wWord,
          swl,
          swr
        ),
        0.U
      ),
      toSQbits.stqEnq.rwReq.wStrb
    )
    .asUInt
  //===================== roStage to Mem2 =============================
  // read from rostage write with exception from rostage, write from SQ should to mem2
  val isSQtoMem2 = (state === storeMode) || (state === ucloadMode && !io.stqEmpty)
  toM2Bits.isSQ            := Mux(isSQtoMem2, true.B, false.B)
  toM2Bits.wbInfo          := roBits.wbInfo
  toM2Bits.prevDstSrc      := roBits.preDstSrc
  toM2Bits.memType         := roBits.memType
  toM2Bits.pTag            := Mux(isSQtoMem2, sqBits.pTag, tlbRes.pTag)
  toM2Bits.isUncache       := CCAttr.isUnCache(Mux(isSQtoMem2, sqBits.cAttr, tlbRes.ccAttr).asUInt)
  toM2Bits.exDetect.happen := Mux(isSQtoMem2, false.B, outEx.happen)

  if (debug) toM2Bits.debugPC.get := Mux(isSQtoMem2, sqBits.debugPC.get, roBits.debugPC.get)
  //======================== Cache Stage 1 ============================
  val cache1 = Module(new CacheStage1(DcachRoads, DcachLineBytes, true))
  cache1.io.in.valid := cache1Update.req
  cache1.io.in.bits.rwReq.get := Mux1H(
    Seq(
      (cache1Update.which === useSQ)   -> wireSq.bits.rwReq,
      (cache1Update.which === useRO)   -> wireRo.bits.rwReq,
      (cache1Update.which === useSelf) -> reCacheRW
    )
  )

  // to mem2
  toM2Bits.toCache2 <> cache1.io.out
  when(!isSQtoMem2) {
    asg(toM2Bits.toCache2.dCacheReq.get, reCacheRW)
    asg(toM2Bits.toCache2.dCacheReq.get.wStrb, strbOfmemType)
    asg(toM2Bits.toCache2.dCacheReq.get.size, sizeOfmemType)
  }.otherwise {
    asg(toM2Bits.toCache2.dCacheReq.get, sqBits.rwReq)
  }
  PipelineConnect(io.fromRO, roDecp, roFireOut, io.flush)
  PipelineConnect(io.fromSQ, sqDecp, sqFireOut, false.B)
  when(roFireOut || io.flush) {
    reCacheOK := false.B
    tlbFinish := false.B
  }

  if (enableCacheInst) {
    cache1.io.in.bits.cacheInst.get <> wireRo.bits.cacheInst.get
    val cio = cache1.io.out.cacheInst.get.bits
    asg(toMem2.bits.toCache2.cacheInst.get, roBits.cacheInst.get)
    // index type cache instr should not require tlb
    // becasue, way infomation is in tag
    when(roBits.cacheInst.get.valid && CacheOp.isIdxInv(cio.op)) {
      io.tlb.req.valid := false.B
    }
    import frontend.ICacheInstIO
    val toICache1 = Wire(Valid(new ICacheInstIO))
    addSource(toICache1, "ICacheInstrReq")
    toICache1.bits.index := cache1.io.out.dCacheReq.get.lowAddr.index
    toICache1.bits.taglo := vaddr
    toICache1.bits.op    := cio.op
    toICache1.valid      := (state === cInstrGo) && CacheOp.isIop(cio.op) && !cInstrExc
  }
  // LL and SC =================================================
  when(roBits.memType === LL && roDecp.valid) { assert(!io.flush) }
  val llRdyGo = WireInit(toMem2.valid && roBits.memType === LL && !outEx.happen)
  addSource(llRdyGo, "llWen")

  val llbitInMem1 = Wire(Bool())
  addSink(llbitInMem1, "llbit")
  toSQbits.scFail := !llbitInMem1 && roBits.memType === SC && !outEx.happen
  toSQbits.isSC   := roBits.memType === SC && !outEx.happen
  val scFailMark = Wire(Valid(UInt(0.W)))
  scFailMark.bits  := DontCare
  scFailMark.valid := toStoreQ.fire && toSQbits.scFail && !io.flush
  addSource(scFailMark, "scFail")

  // WakeUp LSU =================================================
  val wakeUpSource = Wire(Valid(PRegIdx))
  asg(wakeUpSource.bits, toM2Bits.wbInfo.destPregAddr)
  asg(wakeUpSource.valid, toMem2.valid && !toMem2.bits.toCache2.dCacheReq.get.isWrite)

  BoringUtils.addSource(wakeUpSource, "LsuM1WakeUp")
  // Write UART =================================================
  val sqLowAddr = sqDecp.bits.rwReq.lowAddr
  val sqPaddr   = Cat(sqBits.pTag, sqLowAddr.index, sqLowAddr.offset)
  toM2Bits.isWuart := isSQtoMem2 && sqPaddr === "h1faf_fff0".U(32.W)
}
