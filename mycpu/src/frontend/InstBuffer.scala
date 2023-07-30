package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils._
import decodemacro._
import chisel3.util.experimental.BoringUtils

/**
  * implement instbuffer in this stage, can not use queue api
  * because api only support single in and single out
  *
  * move the tail according to the "validNum"
  * write fetchNum insts in instBuffer at one cycle
  * decode the dequeue insts(combination logic) at current cycle
  * but decode logic can not too long because deq is a 16to1mux
  * only decode opcode, src1, src2, dest
  * opcode is used to dispatch select, src and dest are used to rename
  * more complex decode is placed in next cycle
  *
  * To simplify rename and dispatch, InstBuffer only issue 3 or 2 inst,
  * can not chage issue number by decode result
  * in rename stage, if 3 insts can not dispatch or rename for resouce conflict
  * InstBuffer will stop issue until 3 inst has been dispatched
  * it can simplify InstBuffer issue logic from 48to1mux to 16to1 mux
  * the logic of reg should be write in this module
  *
  * out[x].ready are come from one singal, if ready, will issue all valid inst
  */
class InstBuffer extends MycpuModule {
  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(new IfStage2OutIO))
    val out   = Vec(decodeNum, Decoupled(new InstBufferOutIO))
    val flush = Input(Bool())
  })
  // sub decode ==================================================
  val ib = Module(new MultiQueue(fetchNum, decodeNum, new InstBufferEntry, 16, true))

  // avoid icReq(if1) -> iCache data out(if2) -> instbuffer full ->
  // dispatch lsu -> lsu wait iCache instr finish
  asg(ib.io.flush, io.flush)
  // input ========================================================
  (0 until fetchNum).foreach(i => {
    val pushBits = ib.io.push(i).bits
    val inBits   = io.in.bits
    ib.io.push(i).valid    := inBits.validMask(i) && io.in.valid
    pushBits.basicInstInfo := inBits.basicInstInfo(i)
    pushBits.predictResult := inBits.predictResult(i)
    pushBits.exception     := inBits.exception
    pushBits.realBrType    := inBits.realBrType(i)
    pushBits.isBd          := inBits.isBd(i)
    pushBits.isFirPreTake  := inBits.isFirPreTake(i)
  })
  io.in.ready := ib.io.push(0).ready // any number is ok

  // >> Assert ========================================================
  val validMask = io.in.bits.validMask.asUInt
  when(io.in.valid) {
    assert(
      validMask === "b0000".U || validMask === "b0001".U ||
        validMask === "b0011".U || validMask === "b0111".U || validMask === "b1111".U
    )
  }
  val ibRdy = VecInit.tabulate(fetchNum)(i => ib.io.push(i).ready)
  assert(ibRdy(0) === ibRdy(1))
  assume(ibRdy(0) === ibRdy(2))
  assert(ibRdy(0) === ibRdy(3))

  // output ========================================================
  List.tabulate(decodeNum)(i => {
    val outBits = io.out(i).bits
    val ibPop   = ib.io.pop(i).bits
    outBits.basicInstInfo := ibPop.basicInstInfo
    outBits.exception     := ibPop.exception
    outBits.predictResult := ibPop.predictResult
    outBits.realBrType    := ibPop.realBrType
    outBits.isBd          := ibPop.isBd
    outBits.isFirPreTake  := ibPop.isFirPreTake
    io.out(i).valid       := ib.io.pop(i).valid
    ib.io.pop(i).ready    := io.out(i).ready
  })

  @MacroDecode
  class IBdecodeOut extends MycpuBundle {
    val srcType = SRCType()
    val dstType = DSTType()
    val whichFu = ChiselFuType()
  }
  import chisel3.util.experimental.decode.QMCMinimizer
  val subDecode = Wire(Vec(decodeNum, new IBdecodeOut))

  (0 until decodeNum).foreach(i => {
    val outBits      = io.out(i).bits
    val outAregIdx   = outBits.aRegsIdx
    val instr        = outBits.basicInstInfo.instr
    val (rs, rt, rd) = (instr(25, 21), instr(20, 16), instr(15, 11))
    subDecode(i).decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    asg(outBits.whichFu, subDecode(i).whichFu)
    outAregIdx.src0 := LookupEnumDefault(subDecode(i).srcType, rs)(Seq(SRCType.RT -> rt, SRCType.noSRC -> 0.U))
    outAregIdx.src1 := Mux(subDecode(i).srcType === SRCType.RSRT, rt, 0.U)
    outAregIdx.dest := LookupEnum(
      subDecode(i).dstType,
      Seq(DSTType.toRT -> rt, DSTType.toRD -> rd, DSTType.to31 -> 31.U, DSTType.noDST -> 0.U)
    )
  })
}
