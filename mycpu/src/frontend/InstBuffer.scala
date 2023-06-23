package frontend
import chisel3._
import bundle._
import config._
import chisel3.util._
import utils._
import decodemacro._

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
    val in  = Flipped(Decoupled(new IfStage2OutIO))
    val out = Vec(decodeNum, Decoupled(new InstBufferOutIO))
  })
  class InstBufferEntry extends MycpuBundle {
    val predictResult = new PredictResultBundle
    val basicInstInfo = new BasicInstInfoBundle
    val exception     = FrontExcCode()
  }
  // sub decode ==================================================
  val ib = Module(new MultiQueue(fetchNum, decodeNum, new InstBufferEntry, 32, true))
  // input ========================================================
  (0 until fetchNum).foreach(i => {
    ib.io.push(i).valid              := io.in.bits.validMask(i) && io.in.valid
    ib.io.push(i).bits.basicInstInfo := io.in.bits.basicInstInfo(i)
    ib.io.push(i).bits.predictResult := io.in.bits.predictResult(i)
    ib.io.push(i).bits.exception     := io.in.bits.exception
  })
  io.in.ready := ib.io.push(0).ready // any number is ok
  // >> Assert ========================================================
  val validMask = io.in.bits.validMask.asUInt
  assert(
    validMask === "b0000".U || validMask === "b0001".U ||
      validMask === "b0011".U || validMask === "b0111".U || validMask === "b1111".U
  )
  val ibRdy = VecInit.tabulate(fetchNum)(i => ib.io.push(i).ready)
  assert(ibRdy(0) === ibRdy(1))
  assume(ibRdy(0) === ibRdy(2))
  assert(ibRdy(0) === ibRdy(3))
  io.out(0).bits.ROBIdx
  // output ========================================================
  (0 until decodeNum).foreach(i => {
    io.out(i).valid              := ib.io.pop(i).valid
    ib.io.pop(i).ready           := io.out(i).ready
    io.out(i).bits.exception     := ib.io.pop(i).bits.exception
    io.out(i).bits.basic         := ib.io.pop(i).bits.basicInstInfo
    io.out(i).bits.predictResult := ib.io.pop(i).bits.predictResult
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
    val instr = io.out(i).bits.basic.instr
    subDecode(i).decode(instr, AllInsts(), AllInsts.default(), QMCMinimizer)
    asg(io.out(i).bits.whichFu, subDecode(i).whichFu)
    val (rs, rt, rd) = (instr(25, 21), instr(20, 16), instr(15, 11))
    io.out(i).bits.aRegsIdx.src0 := LookupEnumDefault(subDecode(i).srcType, rs)(
      Seq(SRCType.RT -> rt, SRCType.noSRC -> 0.U)
    )
    io.out(i).bits.aRegsIdx.src1 := Mux(subDecode(i).srcType === SRCType.RSRT, rt, 0.U)
    io.out(i).bits.aRegsIdx.dest := LookupEnum(
      subDecode(i).dstType,
      Seq(DSTType.toRT -> rt, DSTType.toRD -> rd, DSTType.to31 -> 31.U, DSTType.noDST -> 0.U)
    )
  })
}
