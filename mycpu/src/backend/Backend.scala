package backend

import chisel3._
import bundle._
import config._
import chisel3.util._
import utils.asg
import utils.PipelineConnect
import frontend._
import cache.TLB

class Backend extends MycpuModule {
  val io = IO(new Bundle {
    val in            = Vec(decodeNum, Flipped(Valid(new InstBufferOutIO)))
    val extInt        = Input(UInt(6.W))
    val fronTlbSearch = Flipped(new TLBSearchIO)

    val dram          = new DramReadIO
    val redirectFront = Flipped(new FrontRedirctIO)
  })

  //component
  val dispatcher = Module(new Dispatcher)
  val arat       = Module(new ArchRAT)
  val rob        = Module(new ROB)
  val mAluRS     = Module(new RS(rsKind = FuType.MainAlu, rsSize = 4))
  val sAluRS     = Module(new RS(rsKind = FuType.SubAlu, rsSize = 4))
  val mduRS      = Module(new RS(rsKind = FuType.Mdu, rsSize = 4))
  val lsuRS      = Module(new RS(rsKind = FuType.Lsu, rsSize = 4))
  val mAluFU     = Module(new Alu(main = true))
  val sAluFU     = Module(new Alu(main = false))
  val lsuFU      = Module(new Lsu)
  val mduFU      = Module(new Mdu)
  val tlb        = Module(new TLB)
  val cp0        = Module(new CP0)

  //variable
  val dperIO            = dispatcher.io
  val (dperIn, dperOut) = (dperIO.in, dperIO.out)
  val (robIn, robOut)   = (rob.io.in, rob.io.out)
  val dperToRs          = List(dperOut.toMainAluRs, dperOut.toSubAluRs, dperOut.toMduRs, dperOut.toLsuRs)
  val rsIO              = List(mAluRS.io, sAluRS.io, mduRS.io, lsuRS.io)
  val fuIO              = List(mAluFU.io, sAluFU.io, lsuFU.io, mduFU.io)
  val fuWb              = (0 until issueNum).map(i => fuIO(i).out)
  val fuIn              = (0 until issueNum).map(i => fuIO(i).in)
  val aluMisPre         = mAluFU.mispre.get
  val flushBackend      = robOut.flushAll || robOut.mispreFlushBackend //flushBackend = robMisFlushBackend & robFlushALL

  /**
    * 4 FU writeback
    *   default:malu->0 sAlu->1 lsu->2
    *   when(!other3 all wb):
    *      mdu select a channel to wb
    *      也可以简单点，限定LSU和MDU共用2号写回口
    */
  val fuWSrat = Wire(Vec(issueNum, new RATWriteBackIO))
  (0 until issueNum).map(i => {
    fuWSrat(i).aDest := fuWb(i).bits.destAregAddr
    fuWSrat(i).pDest := fuWb(i).bits.wPrf.pDest
  })
  //channel：writeBack at most wBNum=3 inst in 1 cycle
  val wPrf  = Vec(wBNum, Valid(new WPrfBundle))
  val wRob  = Vec(wBNum, Valid(new WbRobBundle))
  val wSrat = Vec(wBNum, Valid(new RATWriteBackIO))
  //channel connect
  List.tabulate(wBNum)(i => { //WBNUM=3
    val (wen, wBits) = (fuWb(i).valid, fuWb(i).bits)
    val wSource      = List(wBits.wPrf, wBits.wbRob, fuWSrat(i))
    val wDest        = List(wPrf(i), wRob(i), wSrat(i))
    List.tabulate(wDest.length)(j => {
      asg(wDest(j).bits, wSource(j))
      asg(wDest(j).valid, wen)
    })
    fuWb(i).ready := true.B
  })
  //for mdu wb
  val mduWb = mduFU.io.out
  mduWb.ready := !lsuFU.io.out.valid
  val mduWBits    = List(mduWb.bits.wPrf, mduWb.bits.wbRob, fuWSrat(3))
  val lastChannel = List(wPrf(2), wRob(2), wSrat(2))
  when(!lsuFU.io.out.valid) {
    List.tabulate(mduWBits.length)(i => {
      asg(lastChannel(i).bits, mduWBits(i))
      asg(lastChannel(i).valid, mduWb.valid)
    })
  }

  // val notWenVec = WireInit(VecInit((0 until wBNum).map(i => !fuWb(i).valid))) //malu salu lsu
  // when(notWenVec.asUInt.orR) {
  //   val mduWbSlot = PriorityEncoder(notWenVec)
  //   val wBits     = mduWb.bits
  //   val wSource   = List(wBits.wPrf, wBits.wbRob, fuWSrat(3))
  //   val wDest     = List(wPrf(mduWbSlot), wRob(mduWbSlot), wSrat(mduWbSlot))
  //   List.tabulate(wDest.length)(j => {
  //     asg(wDest(j).bits, wSource(j))
  //     asg(wDest(j).valid, mduWb.valid)
  //   })
  //   mduWb.ready := true.B
  // }

  //dispatcher
  dperIn.fromInstBuffer <> io.in
  asg(dispatcher.io.dsAllow, robOut.dsAllow)
  asg(dperIn.robIndex, robOut.robIndex)
  asg(dperIn.fuWbSrat, wSrat)
  asg(dperIO.recoverSrat.bits, arat.io.recover)
  asg(dperIO.recoverSrat.valid, flushBackend)
  asg(dperIO.fromAluMispre.happen, aluMisPre.happen)
  asg(dperIO.fromAluMispre.realTarget, aluMisPre.realTarget)
  asg(dperIO.pushFl, robOut.flRecover)

  //RS->FU
  (0 until 4).map(i => {
    val rsIn = rsIO(i).in
    rsIn.fromDispatcher <> dperToRs(i)
    rsIn.wPrf         := wPrf
    rsIn.flush        := flushBackend
    rsIn.oldestRobIdx := robOut.oldestIdx
    PipelineConnect(rsIO(i).out, fuIn(i), fuIO(i).roOutFire, flushBackend)
    fuIO(i).flush := flushBackend
  })

  //FU read prf
  val fuSrcPIdx = (0 until issueNum).map(i => fuIn(i).bits.basic.srcPregs)
  val fuRdata   = (0 until issueNum).map(i => fuIO(i).datasFromPrf)
  val raddrs    = Wire(Vec(issueNum, Vec(srcDataNum, PRegIdx)))
  (0 until issueNum).map(i => {
    (0 until srcDataNum).map(j => raddrs(i)(j) := fuSrcPIdx(i)(j).pIdx)
  })
  val rdata = arat.readPrf(raddrs)
  (0 until issueNum).map(i => {
    (0 until srcDataNum).map(j => fuRdata(i)(j) := rdata(i)(j))
  })

  //FU bypass
  val maBpIn = mAluFU.io.bypassIn.get
  asg(maBpIn.valid, sAluFU.io.out.valid)
  asg(maBpIn.bits, sAluFU.io.out.bits.wPrf)
  val saBpIn = sAluFU.io.bypassIn.get
  asg(saBpIn.valid, mAluFU.io.out.valid)
  asg(saBpIn.bits, mAluFU.io.out.bits.wPrf)

  //ROB
  asg(robIn.fromDispatcher, dperOut.toRob)
  asg(robIn.wbRob, wRob)
  asg(robIn.misPredictIdx, aluMisPre.robIdx)
  val mulRe = robOut.multiRetire

  //arat
  val robToArat = Vec(retireNum, Valid(new RATWriteBackIO))
  (0 until retireNum).map(i => {
    robToArat(i).valid := mulRe(i).valid
    robToArat(i).bits  := mulRe(i).bits.toArat
  })
  asg(arat.io.retire, robToArat) //Retire->arat
  asg(arat.io.prfWritePorts, wPrf) //WB->Prf

  //lsu extra
  (0 until retireNum).map(i => asg(lsuFU.scommit(i), mulRe(i).valid && mulRe(i).bits.scommit))
  asg(lsuFU.robOldestIdx, robOut.oldestIdx)
  lsuFU.dram <> io.dram

  //mdu extra
  asg(mduFU.robRetire, robOut.singleRetire)
  mduFU.c0Inst.mfc0 <> cp0.io.mfc0

  //tlb
  io.fronTlbSearch <> tlb.search(0)
  lsuFU.tlb <> tlb.search(1)
  //TODO:

  //cp0
  val cp0In = cp0.io.in
  asg(cp0In.mtc0, mduFU.c0Inst.mtc0)
  asg(cp0In.eretFlush, robOut.eretFlush)
  asg(cp0In.exCommit, robOut.exCommit)
  asg(cp0In.extInt, io.extInt)

  /**
    * redirect frontend
    *   robFlushAll:
    *     robRedirect.flush:CI_next JRHB_Target
    *     exer(target given by CP0)
    *
    * flushBackend = robMisFlushBackend & robFlushALL
    */
  val robRedirect = robOut.robRedirect
  asg(io.redirectFront.flush, robOut.flushAll)
  //发生EXER的时候不会给出robRedirect.flush
  asg(io.redirectFront.target, Mux(robRedirect.flush, robRedirect.target, cp0.io.redirectTarget))
}
