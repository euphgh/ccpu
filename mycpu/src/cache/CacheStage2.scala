package cache

import utils._
import config._
import bundle._
import chisel3._
import chisel3.util._
import utils.BytesWordUtils._
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
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val ptag       = UInt(tagWidth.W)
      val isUncached = Bool()
      val fromStage1 = new CacheStage1OutIO(roads, wordNum, isDcache)
      val cancel     = Bool() // find in SQ or has Exception
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
  val lineNum = math.pow(2, cacheIndexWidth).toInt
  val wordNum = lineBytes / 4
  class RAWBuffer extends MycpuBundle {
    val data  = UWord
    val addr  = UWord
    val valid = Bool()
  }
  // alias ============================================================
  val inBits  = io.in.bits
  val outBits = io.out.bits
  val stage1  = inBits.fromStage1
  val lowAddr = if (isDcache) stage1.dCacheReq.get.lowAddr else stage1.iCacheReq.get
  val dram    = io.dram
  val ar      = io.dram.ar
  val r       = io.dram.r
  val aw      = io.dram.aw
  val w       = io.dram.w
  val b       = io.dram.b
  val dreq    = stage1.dCacheReq.get
  val id      = if (isDcache) "b01".U else "b10".U
  // HIT Logic =================================================================
  val hitMask = VecInit((0 until roads).map(i => {
    val meta = stage1.meta(i)
    (meta.tag === inBits.ptag) && meta.valid
  }))
  val hit = hitMask.asUInt.orR
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
    asg(outBits.idata.get, Mux1H(hitMask, stage1.idata.get)) //default
  }

  // Road Select Module ============================================================
  val roadSelector = ReplacementPolicy.fromString("plru", roads)
  // automat =======================================================================
  val run :: miss :: readDram :: refill :: uncache :: Nil                    = Enum(5)
  val wIdel :: wReq :: wData :: waitwBack :: Nil                             = Enum(4)
  val ucIdel :: ucAReq :: ucRWait :: ucAWReq :: ucWData :: ucWaitBack :: Nil = Enum(6)
  // run -> miss: not hit, waiting
  // miss -> readDram:  ARvalid = 1
  // readDram -> refill:  RReady = 1
  // refill -> run:  sram wen = 1
  //TODO: cache Inst, key word first
  val mainState    = RegInit(run)
  val writeState   = RegInit(wIdel)
  val ucState      = RegInit(ucIdel)
  val writeBuffer  = Reg(Vec(wordNum, UWord))
  val readBuffer   = Reg(Vec(wordNum, UWord))
  val selectedRoad = RegInit(VecInit.fill(4)(false.B))
  val readCounter  = Counter(wordNum)
  val writeCounter = Counter(wordNum)
  val r1data       = Vec(roads, new DPReadBus(UWord, lineNum))
  val w1data       = Vec(roads, new DPWriteBus(UWord, lineNum))
  val w1meta       = Vec(roads, new DPWriteBus(new CacheMeta(isDcache), lineNum))
  val victimRoad   = RegInit(0.U)
  val oldWord      = Mux1H(hitMask, stage1.ddata.get)
  val oldLine      = Mux(mainState === refill, writeBuffer, Mux1H(hitMask, stage1.dataline.get))
  val newWord      = maskWord(dreq.wWord, dreq.wStrb).asUInt | maskWord(oldWord, ~dreq.wStrb).asUInt
  val newLine = LookupUInt(
    lowAddr.offset >> 2,
    (0 until wordNum).map(i => {
      i.U -> Cat(
        VecInit((i + 1 until wordNum).map(oldLine(_))).asUInt,
        newWord,
        VecInit((0 until i).map(oldLine(_))).asUInt
      )
    })
  )
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
    r1data(i).req.valid       := false.B
    r1data(i).req.bits.setIdx := lowAddr.index
    w1data(i).req.valid       := false.B
    // not default but for permenant assign
    asg(w1meta(i).req.bits.data.tag, inBits.ptag)
    asg(w1meta(i).req.bits.data.valid, true.B)
    asg(w1meta(i).req.bits.setIdx, lowAddr.index)
    asg(w1data(i).req.bits.data, newLine)
  })
  // Default Bus Assign ========================================================
  // >> AR channel =============================================================
  asg(ar.bits.addr, Cat(inBits.ptag, lowAddr.index, 0.U(cacheOffsetWidth.W)))
  asg(ar.bits.burst, BurstType.INCR) //TODO: key word first
  asg(ar.bits.size, SizeType.Word)
  asg(ar.bits.len, (wordNum - 1).U)
  asg(ar.bits.id, id)
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
  asg(aw.bits.size, SizeType.Word)
  asg(aw.bits.len, (wordNum - 1).U)
  asg(ar.bits.id, id)
  // >> W channel ==============================================================
  w.bits.id   := id
  w.bits.strb := "b1111".U

  switch(mainState) {
    is(run) {
      // set refill write sram valid = false.B
      w1data(victimRoad).req.valid := false.B
      w1meta(victimRoad).req.valid := false.B
      // default: cancel || unvalid
      io.in.ready  := io.out.ready
      io.out.valid := io.in.valid
      when(!inBits.cancel && io.in.valid) {
        when(inBits.isUncached) {
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
          io.out.valid := true.B
          if (isDcache) {
            (0 until roads).foreach(i => {
              w1data(i).req.valid := hitMask(i) && dreq.isWrite
            })
          }
        }.otherwise {
          mainState := miss
          // block when not hit
          io.out.valid := false.B
          io.in.ready  := false.B
          // perpare data for next miss state
          (0 until roads).foreach(r1data(_).req.valid := true.B)
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
      // read 4(roads) cachelines for replace
      asg(
        readBuffer,
        LookupUInt(
          victimRoad,
          (0 until roads).map(i => {
            i.U -> r1data(i).resp.data
          })
        )
      )
      (0 to roads).map(i => {
        r1data(i).req.valid := false.B
      })
      // tell writeBuffer start work
      writeState := wReq
      assert(writeState === wIdel)
    }
    is(readDram) {
      dram.whenRfire {
        readBuffer(readCounter.value) := r.bits.data
        readCounter.inc()
        assert(r.bits.last && readCounter.value === (wordNum - 1).U || !r.bits.last)
        mainState := Mux(r.bits.last, refill, readDram)
      }
    }
    is(refill) {
      // req ok, give result
      io.out.valid := true.B
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
        mainState := run
      }
      // write axi back data to cache data and metas
      w1data(victimRoad).req.valid := true.B
      w1meta(victimRoad).req.valid := true.B
    }
    is(uncache) {
      when(ucState === ucIdel) {
        io.out.valid := true.B
        if (isDcache) {
          outBits.ddata.get := ucDBuffer.get
        } else {
          outBits.idata.get := ucIBuffer.get
          // miacro decode
          (0 until fetchNum).foreach(i => {
            outBits.toUser(i) := trans(ucIBuffer.get(i))
          })
        }
        mainState := Mux(io.out.ready, run, uncache)
      }
    }
  }
  assert(io.in.valid || mainState === run)
  assert(io.in.ready === false.B || mainState === run)

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
        asg(w.bits.data, writeBuffer(writeCounter.value))
        writeState := Mux(w.bits.last, waitwBack, wData)
      }
    }
    is(waitwBack) {
      dram.whenBfire {
        writeBuffer := wIdel
      }
    }
  }
  val ucICounter = if (!isDcache) Some(Counter(fetchNum)) else None
  val ucIBuffer  = if (!isDcache) Some(Vec(fetchNum, UWord)) else None
  val ucDBuffer  = if (isDcache) Some(UWord) else None
  switch(ucState) {
    // UnCache Read Channel ===========================
    is(ucAReq) {
      // axi bus
      ar.bits.addr  := Cat(inBits.ptag, lowAddr.index, lowAddr.offset)
      ar.bits.burst := BurstType.INCR
      ar.bits.size  := SizeType.Word
      ar.bits.len   := (if (isDcache) 4.U else 0.U)
      ar.bits.id    := id
      // wait until ar.fire
      io.dram.whenARfire {
        ucState := ucRWait
      }
      // instr fetch need burst
      if (!isDcache) {
        ucICounter.get.reset()
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
          assert(r.bits.last && icounter.value === (fetchNum - 1).U || !r.bits.last)
          ucState := Mux(r.bits.last, ucIdel, ucRWait)
        }
      }
    }
    // UnCache Write Channel ==========================
    is(ucAWReq) {
      aw.bits.addr  := Cat(inBits.ptag, lowAddr.index, lowAddr.offset)
      aw.bits.burst := BurstType.INCR
      aw.bits.size  := SizeType.Word
      aw.bits.len   := 0.U
      ar.bits.id    := id
      dram.whenAWfire {
        ucState := ucRWait
      }
      if (!isDcache) assert(false.B)
    }
    is(ucWData) {
      w.bits.id   := id
      w.bits.strb := dreq.wStrb
      w.bits.last := true.B
      dram.whenWfire {
        asg(w.bits.data, dreq.wWord)
        writeState := Mux(w.bits.last, waitwBack, wData)
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
}
