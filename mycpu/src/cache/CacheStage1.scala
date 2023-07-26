package cache

import utils._
import config._
import config.MycpuInit.PCReset
import bundle._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils._

/**
  * I-cache stage1 not block, it always give data in the cycle next to in.valid cycle
  * if I-cache stage2 not ready, stage1 will lose old data by writing new data
  * so it in.ready can always be true
  * but D-cache stage1 may block for stage 2 not ready
  *
  * in. {size, wWord, sStrb} is from store queue, will be pass to next stage
  *
  * in.memType is from Decode, will be calculate, and turn to out. {isWrite, loadSel}
  *
  * stage1 has 4 8to1 mux, select word from SRAM line by offset
  *
  * cacheInst.op.valid meaning this is a Cache Inst
  * D-stage1 only need pass op and read meta for D-stage2
  * D-stage1 is always not block for no inst in pipeline
  * tlb should read from mem unit to pass D-cache stage2
  * if exception happen, D-cache stage2 should not do any thing
  * set OK D-cache Inst signal in next cycle
  *
  * I-cache stage1 should accept it like flush, no matter it has data or not
  * it can be pass by by `addSink` and `addSource` from mem unit
  * same time, D-stage1 accept a "fake" cacheInstOp meaning reflect I-cache
  * this design is for I-cacheInst can flow like D-cacheInst in pipeline
  * tlb should be read from D-tlb and I-tlb,
  * when I-tlb pass it to I-stage2, D-tlb pass exception to D-stage2
  * if tlb has exception I-stage2 perform like D-stage2, doing nothing
  * set OK I-cache Inst signal in next cycle
  * when D-stage2 accept the "fake" cacheInstOp, it listens and reflect
  * I-stage2's finish signal by `addSink` and `addSource` function.
  */
class CacheStage1(
  roads:     Int     = 4,
  lineBytes: Int     = 32,
  isDcache:  Boolean = false)
    extends MycpuModule {
  val lineNum = math.pow(2, cacheIndexWidth).toInt
  val wordNum = lineBytes / 4
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new CacheStage1In(isDcache)))
    val out = new CacheStage1OutIO(roads, wordNum, isDcache)
  })
  // Reg stage
  io.in.ready := true.B // ifstage1 and mem1 should keep
  val searchIndex = Wire(UInt(cacheIndexWidth.W)) // becasue bram should take one cycle
  val stageReg = RegEnable(
    io.in.bits, {
      val init = WireInit(0.U.asTypeOf(new CacheStage1In(isDcache)))
      if (!isDcache) {
        asg(init.ifReq.get.index, PCReset(cacheIndexWidth + cacheOffsetWidth - 1, cacheOffsetWidth))
        asg(init.ifReq.get.offset, PCReset(cacheOffsetWidth - 1, 0))
      }
      init
    },
    io.in.valid
  )
  if (!isDcache) {
    asg(searchIndex, Mux(io.in.valid, io.in.bits.ifReq.get.index, stageReg.ifReq.get.index))
  } else {
    asg(searchIndex, Mux(io.in.valid, io.in.bits.rwReq.get.lowAddr.index, stageReg.rwReq.get.lowAddr.index))
  }
  val r2data = List.fill(roads)(Wire(Flipped(new DPReadBus(Vec(wordNum, UWord), lineNum))))
  val w2data = List.fill(roads)(Wire(Flipped(new DPWriteBus(Vec(wordNum, UWord), lineNum))))
  val w2meta = List.fill(roads)(Wire(Flipped(new DPWriteBus(new CacheMeta(isDcache), lineNum))))
  val metas  = List.fill(roads)(Module(new DPTemplate(new CacheMeta(isDcache), lineNum, true)))
  val datas  = List.fill(roads)(Module(new DPTemplate(Vec(wordNum, UWord), lineNum, true)))
  (0 until roads).foreach(i => {
    r2data(i).req.ready := true.B
    if (isDcache) {
      addSink(r2data(i), s"DcacheStage2ReadData$i")
      addSink(w2data(i), s"DcacheStage2WriteData$i")
      addSink(w2meta(i), s"DcacheStage2WriteMeta$i")
    } else {
      addSink(r2data(i), s"IcacheStage2ReadData$i")
      addSink(w2data(i), s"IcacheStage2WriteData$i")
      addSink(w2meta(i), s"IcacheStage2WriteMeta$i")
    }
    metas(i).io.r(true.B, searchIndex)
    datas(i).io.r(true.B, Mux(r2data(i).req.valid, r2data(i).req.bits.setIdx, searchIndex))
    r2data(i).resp := datas(i).io.r.resp
    val stageIndex      = if (isDcache) stageReg.rwReq.get.lowAddr.index else stageReg.ifReq.get.index
    val writeFromStage2 = w2data(i).req.valid && (w2data(i).req.bits.setIdx === stageIndex)
    assert(Mux(w2data(i).req.valid, w2meta(i).req.valid, true.B))
    datas(i).io.w <> w2data(i)
    metas(i).io.w <> w2meta(i)
    val metasOut = Mux(writeFromStage2, w2meta(i).req.bits.data, metas(i).io.r.resp.data)
    val datasOut = Mux(writeFromStage2, w2data(i).req.bits.data, datas(i).io.r.resp.data)
    // out mates ====================================================
    io.out.meta(i) := metasOut
    // out datas and req ============================================
    if (isDcache) {
      // offset should delay
      val selOffset = stageReg.rwReq.get.lowAddr.offset
      asg(
        io.out.ddata.get(i),
        LookupUInt(
          selOffset >> 2,
          (0 until wordNum).map(j => {
            j.U -> datasOut(j)
          })
        )
      )
      asg(io.out.dataline.get(i), datasOut)
      asg(io.out.dCacheReq.get, stageReg.rwReq.get)
    } else {
      val selOffset = stageReg.ifReq.get.offset
      asg(
        io.out.idata.get(i),
        LookupUInt(
          selOffset >> 2,
          (0 until wordNum).map(j => {
            val dataLine = datasOut
            j.U -> VecInit(
              dataLine((j + 0) % wordNum),
              dataLine((j + 1) % wordNum),
              dataLine((j + 2) % wordNum),
              dataLine((j + 3) % wordNum)
            )
          })
        )
      )
      asg(io.out.iCacheReq.get, stageReg.ifReq.get)
    }
    if (enableCacheInst) {
      io.out.cacheInst.get := stageReg.cacheInst.get
    }
  })
}
