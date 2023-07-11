package cache

import utils._
import config._
import bundle._
import chisel3._
import chisel3.util._
import utils.BytesWordUtils._
import utils._
import chisel3.util.experimental.BoringUtils._

/**
  * cache stage2 may block, need Decoupled input
  * output is Decoupled becasue instBuffer
  *
  * stage2 will calculate hit or miss
  * tag, isUncache, isException can only be known after TLB
  * if isUncache, cache must access DRAM
  * if isException, cache must return valid in next cycle and not change Cache status
  *
  * userGen is only used to pre-Decode in I-Cache
  * if no pre-Decode or is Dcache, can pass userGen UInt(0.W)
  * stage2 call trans func to pre-Decode after roads selection
  *
  * pay attention !!!
  * out.data in I-cache is four word(inst)
  * out.data is not load bytes in Dcache, but "align word"
  *
  * in.fromStage1.cacheOp.valid meaning that it cache Inst
  * stage2 should only take corresponding action by input
  * note that if isException must do nothing and set OK signal
  * when stage2 set cacheInst OK signal,
  * it not set ready signal for preventing cache pollution
  * it waiting redirect signal to set it ready
  */
class CacheStage2[T <: Data](
  val roads:     Int         = 4,
  val lineBytes: Int         = 8,
  val isDcache:  Boolean     = false,
  val userGen:   T           = UInt(0.W)
)(val trans:     (UInt => T) = (x: UInt) => { 0.U })
    extends MycpuModule {
  val lineNum = math.pow(2, cacheIndexWidth).toInt
  val wordNum = lineBytes / 4
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val ptag       = UInt(tagWidth.W)
      val isUncached = Bool()
      val fromStage1 = new CacheStage1OutIO(roads, wordNum, isDcache)
      val cancel     = Bool() // find in SQ or has Exception
      val imask      = if (!isDcache) Some(Vec(fetchNum, Bool())) else None
    }))
    val out = Decoupled(new Bundle {
      val toUser = Output(Vec(fetchNum, userGen))
      val idata  = if (!isDcache) Some(Output(Vec(fetchNum, UWord))) else None
      val ddata  = if (isDcache) Some(Output(UWord)) else None
    })
    val cacheInst = new Bundle {
      val finish   = if (enableCacheInst) Some(Output(Bool())) else None
      val redirect = if (enableCacheInst) Some(Input(Bool())) else None
    }
    val dram = new DramIO
  })

  // alias and utils ============================================================
  val inBits      = io.in.bits
  val outBits     = io.out.bits
  val stage1      = inBits.fromStage1
  val lowAddr     = if (isDcache) stage1.dCacheReq.get.lowAddr else stage1.iCacheReq.get
  val dram        = io.dram
  val ar          = io.dram.ar
  val r           = io.dram.r
  val aw          = io.dram.aw
  val w           = io.dram.w
  val b           = io.dram.b
  val dreq        = stage1.dCacheReq.getOrElse(0.U.asTypeOf(new CacheRWReq))
  val id          = if (isDcache) "b0001".U(4.W) else "b0010".U(4.W)
  val isCacheInst = stage1.cacheInst.fold(false.B)(_.valid)
  val imask       = io.in.bits.imask.fold(0.U)(_.asUInt)
  val ivalidNum   = PriorityCount(imask) //count how much instr is valid
  def dirtyMeta(meta: CacheMeta) = {
    val newMeta = WireInit(meta)
    newMeta.dirty.get := true.B
    newMeta
  }
  def unvalidMeta(meta: CacheMeta) = {
    val newMeta = WireInit(meta)
    newMeta.valid := false.B
    newMeta
  }
  def selectMetasByWay(way: UInt) = {
    LookupUInt(
      way,
      (0 until roads).map(i => {
        i.U -> stage1.meta(i)
      })
    )
  }
  // HIT Logic =================================================================
  val hitMask = VecInit((0 until roads).map(i => {
    val meta = stage1.meta(i)
    (meta.tag === inBits.ptag) && meta.valid
  }))
  val hit = hitMask.asUInt.orR
  io.out.bits.toUser.foreach(_ := DontCare)
  if (isDcache) {
    asg(outBits.ddata.get, Mux1H(hitMask, stage1.ddata.get)) //default
  } else {
    // miscro decode
    (0 until fetchNum).foreach { i =>
      io.out.bits.toUser(i) := Mux1H(
        hitMask,
        (0 until roads).map(j => {
          trans(stage1.idata.get(j)(i))
        })
      )
    } // default
    asg(
      outBits.idata.get,
      Mux(inBits.cancel, VecInit.fill(fetchNum)(0.U(32.W)), Mux1H(hitMask, stage1.idata.get))
    ) //default
  }

  // Road Select Module ============================================================
  val roadSelector = ReplacementPolicy.fromString("plru", roads)
  // automat =======================================================================
  val run :: miss :: readDram :: refill :: uncache :: instr :: Nil                                  = Enum(6)
  val wIdel :: wReq :: wData :: waitwBack :: Nil                                                    = Enum(4)
  val ucIdel :: ucAReq :: ucRWait :: ucAWReq :: ucWData :: ucWaitBack :: Nil                        = Enum(6)
  val instrIdle :: decode :: idxStTag :: hitInv :: idxInv :: fake :: waitWauto :: waitRetire :: Nil = Enum(8)
  // run -> miss: not hit, waiting
  // miss -> readDram:  ARvalid = 1
  // readDram -> refill:  RReady = 1
  // refill -> run:  sram wen = 1
  //TODO: cache Inst, key word first
  val mainState    = RegInit(run)
  val writeState   = RegInit(wIdel)
  val ucState      = RegInit(ucIdel)
  val instrState   = RegInit(ucIdel)
  val wbBuffer     = Reg(Vec(wordNum, UWord))
  val readBuffer   = Reg(Vec(wordNum, UWord))
  val readCounter  = Counter(wordNum)
  val writeCounter = Counter(wordNum)
  val r1data       = Wire(Vec(roads, new DPReadBus(Vec(wordNum, UWord), lineNum)))
  val w1data       = Wire(Vec(roads, new DPWriteBus(Vec(wordNum, UWord), lineNum)))
  val w1meta       = Wire(Vec(roads, new DPWriteBus(new CacheMeta(isDcache), lineNum)))
  val victimRoad   = RegInit(0.U)
  val validDirty =
    if (isDcache) VecInit((0 until roads).map(i => stage1.meta(i).dirty.get && stage1.meta(i).valid)).asUInt
    else 0.U(roads.W)
  val newLine = WireInit(0.U((dataWidth * wordNum).W)) //init
  if (isDcache) {
    val oldWord = Mux1H(hitMask, stage1.ddata.get)
    val oldLine = Mux(mainState === refill, readBuffer, Mux1H(hitMask, stage1.dataline.get))
    val newWord = maskWord(dreq.wWord, dreq.wStrb).asUInt | maskWord(oldWord, ~dreq.wStrb).asUInt
    val wordSel = lowAddr.offset >> 2
    asg(
      newLine,
      MuxCase(
        Cat(newWord, VecInit((0 until wordNum - 1).map(oldLine(_))).asUInt), //111->(newword,6..0)
        Seq(
          (wordSel === 0.U) -> Cat(VecInit((1 until wordNum).map(oldLine(_))).asUInt, newWord), ///000->(7..1,newword)
          (wordSel =/= (wordNum - 1).U) -> LookupUInt(
            wordSel,
            (1 until wordNum - 1).map(i => {
              i.U -> Cat(
                VecInit((i + 1 until wordNum).map(oldLine(_))).asUInt,
                newWord,
                VecInit((0 until i).map(oldLine(_))).asUInt
              )
            })
          )
        )
      )
    )
  }
  val newLineVec = Wire(Vec(wordNum, UWord))
  (0 until wordNum).map(i => newLineVec(i) := newLine((i + 1) * 32 - 1, i * 32))

  (0 until roads).foreach(i => {
    if (isDcache) {
      addSource(r1data(i), s"DcacheStage2ReadData$i")
      addSource(w1data(i), s"DcacheStage2WriteData$i")
      addSource(w1meta(i), s"DcacheStage2WriteMeta$i")
    } else {
      addSource(r1data(i), s"IcacheStage2ReadData$i")
      addSource(w1data(i), s"IcacheStage2WriteData$i")
      addSource(w1meta(i), s"IcacheStage2WriteMeta$i")
    }
    // default
    r1data(i).req.valid := false.B
    w1data(i).req.valid := false.B
    w1meta(i).req.valid := false.B
    // not default but for permenant assign
    asg(w1meta(i).req.bits.setIdx, lowAddr.index)
    asg(r1data(i).req.bits.setIdx, lowAddr.index)
    asg(w1data(i).req.bits.setIdx, lowAddr.index)
    asg(w1meta(i).req.bits.data.tag, inBits.ptag)
    asg(w1meta(i).req.bits.data.valid, true.B)
    // icache: refill read, dcache: refill read refill write, hit write
    // when refill read will readbuffer, refill write is newline, write hit is newline
    if (isDcache) asg(w1data(i).req.bits.data, newLineVec)
    else asg(w1data(i).req.bits.data, readBuffer)
    if (isDcache) asg(w1meta(i).req.bits.data.dirty.get, false.B)
  })
  // Default Bus Assign ========================================================
  // >> AR channel =============================================================
  asg(ar.bits.addr, Cat(inBits.ptag, lowAddr.index, 0.U(cacheOffsetWidth.W)))
  asg(ar.bits.burst, BurstType.INCR) //TODO: key word first
  asg(ar.bits.size, SizeType.Word.asUInt)
  asg(ar.bits.len, (wordNum - 1).U(4.W))
  asg(ar.bits.id, id)
  asg(ar.valid, false.B)
  asg(r.ready, true.B)
  // >> AW channel ==============================================================
  asg(
    aw.bits.addr,
    Cat(
      LookupUInt(victimRoad, (0 until roads).map(i => i.U -> stage1.meta(i).tag)),
      lowAddr.index,
      0.U(cacheOffsetWidth.W)
    )
  )
  asg(aw.bits.burst, BurstType.INCR)
  asg(aw.bits.size, SizeType.Word.asUInt)
  asg(aw.bits.len, (wordNum - 1).U(4.W))
  asg(aw.bits.id, id)
  asg(aw.valid, false.B)
  // >> W channel ==============================================================
  asg(w.bits.id, id)
  asg(w.bits.strb, "b1111".U)
  asg(w.valid, false.B)
  w.bits.data := DontCare
  w.bits.last := DontCare
  // >> B channel ==============================================================
  b.ready := true.B

  val firstRefillCycle = RegInit(false.B) // only first can write Cache data and meta

  val firstMissCycle = RegInit(false.B) // only first can change write state
  val ucICounter     = if (!isDcache) Some(Counter(fetchNum)) else None
  val ucIBuffer      = if (!isDcache) Some(RegInit(VecInit.fill(fetchNum)(0.U(32.W)))) else None
  val ucDBuffer      = if (isDcache) Some(RegInit(0.U(32.W))) else None
  io.in.ready  := false.B // default, only in run state assign
  io.out.valid := false.B // defualt, only in run state assign
  switch(mainState) {
    is(run) {
      // set refill write sram valid = false.B
      w1data(victimRoad).req.valid := false.B
      w1meta(victimRoad).req.valid := false.B
      // default: cancel || unvalid
      io.in.ready  := io.out.ready
      io.out.valid := io.in.valid
      when(!inBits.cancel && io.in.valid) {
        when(isCacheInst) {
          mainState := instr
        }.elsewhen(inBits.isUncached) {
          mainState := uncache
          // Uncache automachine change
          assert(ucState === ucIdel)
          if (isDcache) {
            ucState := Mux(dreq.isWrite, ucAWReq, ucAReq)
          } else ucState := ucAReq
          // block when uncache
          io.in.ready  := false.B
          io.out.valid := false.B
        }.elsewhen(hit) {
          // state not change, not block when not uncache and hit
          io.in.ready  := io.out.ready
          io.out.valid := io.in.valid
          if (isDcache) {
            (0 until roads).foreach(i => {
              w1data(i).req.valid     := hitMask(i) && dreq.isWrite
              w1meta(i).req.valid     := hitMask(i) && dreq.isWrite
              w1meta(i).req.bits.data := dirtyMeta(inBits.fromStage1.meta(i))
            })
          }
        }.otherwise { //cache not hit
          mainState := miss
          // calculate and write next victim way to way status
          roadSelector.miss
          firstMissCycle := true.B
          // block when not hit
          io.out.valid := false.B
          io.in.ready  := false.B
          // perpare data for next miss state
          (0 until roads).foreach(r1data(_).req.valid := true.B)
        }
        if (!isDcache) {
          when(io.in.valid && !isCacheInst) {
            assert(PriorityCount.consecutive(imask))
            assert(imask.asUInt =/= 0.U)
          }
        }
      }
    }
    is(miss) {
      dram.whenARfire {
        mainState := readDram
      }
      // burst count clear
      readCounter.reset()
      // select road, save result reg
      victimRoad := roadSelector.way

      (0 until roads).map(i => { r1data(i).req.valid := false.B })
      // when victim is dirty, tell writeBuffer start work
      // must on first cycle, can only write one times
      // write to wbBuffer must on first data
      when(firstMissCycle) {
        writeState     := Mux(validDirty(victimRoad), wReq, wIdel)
        firstMissCycle := false.B
        assert(writeState === wIdel)
        // read 4(roads) cachelines for replace
        asg(
          wbBuffer,
          LookupUInt(
            victimRoad,
            (0 until roads).map(i => {
              i.U -> r1data(i).resp.data
            })
          )
        )
      }
    }
    is(readDram) {
      dram.whenRfire {
        readBuffer(readCounter.value) := r.bits.data
        readCounter.inc()
        assert(r.bits.last && readCounter.value === (wordNum - 1).U || !r.bits.last)
        when(r.bits.last) {
          mainState        := refill
          firstRefillCycle := true.B
        }
      }
    }
    is(refill) {
      // false first refill
      firstRefillCycle := false.B
      // req ok, give result
      io.out.valid := io.in.valid
      // select data by offset
      if (isDcache) {
        asg(
          outBits.ddata.get,
          LookupUInt(
            lowAddr.offset >> 2,
            (0 until wordNum).map(i => {
              i.U -> readBuffer(i)
            })
          )
        )
      } else {
        asg(
          outBits.idata.get,
          LookupUInt(
            lowAddr.offset >> 2,
            (0 until wordNum).map(i => {
              val dataLine = readBuffer
              i.U -> VecInit(
                dataLine(i),
                dataLine((i + 1) % wordNum),
                dataLine((i + 2) % wordNum),
                dataLine((i + 3) % wordNum)
              )
            })
          )
        )
      }
      // wait write ok to get next req
      when(writeState === wIdel) {
        mainState   := Mux(io.out.fire || !io.in.valid, run, refill)
        io.in.ready := io.out.ready
      }
      // write axi back data to cache data and metas
      // must first cycle can write, else inBits will not valid
      w1data(victimRoad).req.valid := true.B && firstRefillCycle
      w1meta(victimRoad).req.valid := true.B && firstRefillCycle
      if (isDcache) {
        when(!dreq.isWrite) {
          asg(w1data(victimRoad).req.bits.data, readBuffer)
        }
      } else {
        asg(w1data(victimRoad).req.bits.data, readBuffer)
      }
    }
    is(uncache) {
      when(ucState === ucIdel) {
        io.out.valid := io.in.valid
        io.in.ready  := io.out.ready
        if (isDcache) {
          outBits.ddata.get := ucDBuffer.get
        } else {
          outBits.idata.get := ucIBuffer.get
          // miacro decode
          (0 until fetchNum).foreach(i => {
            outBits.toUser(i) := trans(ucIBuffer.get(i))
          })
        }
        mainState := Mux(io.out.fire || !io.in.valid, run, uncache)
      }
    }
    is(instr) {
      instrState := decode
      // not return run there, until instrState set mainState to run
    }
  }
  val canReady = (mainState === run) || (mainState === refill && writeState === wIdel)
  val canValid = (mainState === run) || (mainState === refill)
  // assert(Mux(io.out.valid, canValid, true.B))
  // assert(Mux(io.in.ready, canReady, true.B))
  assert(!(io.out.valid && !io.in.valid))
  assert(!(io.in.ready && !io.out.ready))

  switch(writeState) {
    is(wReq) {
      dram.whenAWfire {
        writeState := wData
      }
      writeCounter.reset()
    }
    is(wData) {
      w.bits.last := writeCounter.value === (wordNum - 1).U
      dram.whenWfire {
        writeCounter.inc()
        asg(w.bits.data, wbBuffer(writeCounter.value))
        writeState := Mux(w.bits.last, waitwBack, wData)
      }
    }
    is(waitwBack) {
      dram.whenBfire {
        writeState := wIdel
      }
    }
  }

  switch(ucState) {
    // UnCache Read Channel ===========================
    is(ucAReq) {
      // axi bus
      ar.bits.addr  := Cat(inBits.ptag, lowAddr.index, lowAddr.offset)
      ar.bits.burst := BurstType.INCR
      ar.bits.size  := SizeType.Word.asUInt
      ar.bits.len   := (if (!isDcache) ivalidNum - 1.U else 0.U)
      ar.bits.id    := id
      // wait until ar.fire
      io.dram.whenARfire {
        ucState := ucRWait
      }
      // instr fetch need burst
      if (isDcache) {
        ucDBuffer.get := 0.U(32.W)
      } else {
        ucICounter.get.reset()
        ucIBuffer.get := VecInit.fill(fetchNum)(0.U(32.W))
      }
    }
    is(ucRWait) {
      // wait until r.fire
      io.dram.whenRfire {
        if (isDcache) {
          ucState       := ucIdel
          ucDBuffer.get := r.bits.data
          assert(dram.r.bits.last)
        } else {
          val icounter = ucICounter.get
          ucIBuffer.get(icounter.value) := r.bits.data
          icounter.inc()
          val finish = icounter.value === ivalidNum - 1.U
          assert(r.bits.last && finish || !r.bits.last)
          ucState := Mux(r.bits.last, ucIdel, ucRWait)
        }
      }
    }
    // UnCache Write Channel ==========================
    is(ucAWReq) {
      aw.bits.addr  := Cat(inBits.ptag, lowAddr.index, lowAddr.offset)
      aw.bits.burst := BurstType.INCR
      aw.bits.size  := SizeType.Word.asUInt
      aw.bits.len   := 0.U
      ar.bits.id    := id
      dram.whenAWfire {
        ucState := ucWData
      }
      if (!isDcache) assert(false.B)
    }
    is(ucWData) {
      w.bits.id   := id
      w.bits.strb := dreq.wStrb
      w.bits.last := true.B
      dram.whenWfire {
        asg(w.bits.data, dreq.wWord)
        ucState := Mux(w.bits.last, ucWaitBack, ucWData)
      }
      if (!isDcache) assert(false.B)
    }
    is(ucWaitBack) {
      dram.whenBfire {
        ucState := ucIdel
      }
      if (!isDcache) assert(false.B)
    }
  }
  // Cache Inst ======================================================
  if (enableCacheInst) {

    def invalidWriteBack(way: UInt, isWB: Bool) = {
      // unvalid tag meta
      w1meta(way).req.valid     := true.B
      w1meta(way).req.bits.data := unvalidMeta(selectMetasByWay(way))
      // Dcache condition write back
      if (isDcache) {
        // load cache line data to wb buffer
        asg(
          wbBuffer,
          LookupUInt(
            way,
            (0 until roads).map(i => {
              i.U -> r1data(i).resp.data
            })
          )
        )
        // start write back automation conditionally
        val needWriteBack = validDirty(way) && isWB
        writeState := Mux(needWriteBack, wReq, wIdel)
        assert(writeState === wIdel)
        instrState := Mux(needWriteBack, waitWauto, waitRetire)
      } else instrState := waitRetire // ICache instr not wait write back
    }

    val tagWay            = inBits.ptag(cacheIndexWidth + log2Ceil(roads) - 1, cacheIndexWidth)
    val ciOp              = io.in.bits.fromStage1.cacheInst.get.bits.op
    val iCacheFinishInstr = WireInit(false.B) // for reflect icache finish to dcache
    if (isDcache) {
      addSink(iCacheFinishInstr, "iCacheFinishInstr")
    }
    if (!isDcache) {
      addSource(io.cacheInst.finish.get, "iCacheFinishInstr")
    }
    io.cacheInst.finish.get := false.B
    assert(io.in.valid || instrState === instrIdle)
    switch(instrState) {
      is(decode) {
        if (isDcache) {
          instrState := MuxCase(
            decode,
            Seq(
              CacheOp.isIdxInv(ciOp)      -> idxInv,
              CacheOp.isIdxStoreTag(ciOp) -> idxStTag,
              CacheOp.isHitInv(ciOp)      -> hitInv,
              CacheOp.isIop(ciOp)         -> fake
            )
          )
        } else {
          assert(CacheOp.isIop(ciOp))
          instrState := MuxCase(
            decode,
            Seq(
              CacheOp.isIdxInv(ciOp)      -> idxInv,
              CacheOp.isIdxStoreTag(ciOp) -> idxStTag,
              CacheOp.isHitInv(ciOp)      -> hitInv
            )
          )
        }
        // perpare data for write back instr
        (0 until roads).foreach(r1data(_).req.valid := true.B)
      }
      is(idxInv) {
        invalidWriteBack(tagWay, true.B)
        assert(ciOp === CacheOp.IndexInvalidI || ciOp === CacheOp.IndexWriteBackInvalidD)
      }
      is(hitInv) {
        //defualt not hit
        instrState := instrIdle
        when(hit) {
          invalidWriteBack(OHToUInt(hitMask), ciOp === CacheOp.HitWriteBackInvalidD)
        }
        assert(ciOp === CacheOp.HitInvalidD || ciOp === CacheOp.HitInvalidI || ciOp === CacheOp.HitWriteBackInvalidD)
      }
      is(idxStTag) {
        // unvalid tag meta
        w1meta(tagWay).req.valid     := true.B
        w1meta(tagWay).req.bits.data := 0.U.asTypeOf(new CacheMeta(isDcache))
        instrState                   := waitRetire
        assert(ciOp === CacheOp.IndexStoreTagI || ciOp === CacheOp.IndexStoreTagD)
      }
      is(waitWauto) {
        instrState := Mux(writeState === waitRetire, instrIdle, waitWauto)
      }
      is(waitRetire) {
        io.cacheInst.finish.get := true.B
        when(io.cacheInst.redirect.get) {
          mainState  := run
          instrState := instrIdle
        }
      }
      is(fake) { // when Dcache recieve ICache Instr, it should listen
        if (isDcache) {
          io.cacheInst.finish.get := iCacheFinishInstr
          when(iCacheFinishInstr) {
            instrState := waitRetire
          }
        }
      }
    }
  }
}
