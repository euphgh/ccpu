package backend
import bundle._
import config._
import chisel3._
import chisel3.util._
import utils.asg
import utils.BytesWordUtils._
import difftest.DifftestArchIntRegState

/**
  * connect rs.out.renamed.srcPregs to prf.readPorts.addr in Backend
  * the readData connect to FU
  *
  * connect FU.wprf to here in Backend
  *     the wb width is 3,now we dicide to block mdu_out if other 3 all produce
  */
class PrfBundle extends MycpuBundle {
  val readPorts = Vec(
    issueNum,
    Vec(
      srcDataNum,
      new Bundle {
        val addr = Input(PRegIdx)
        val data = Output(UWord)
      }
    )
  )
  val writePorts = Vec(wBNum, Flipped(Valid(new WPrfBundle)))
}

class Prf extends MycpuModule {
  val io = IO(new PrfBundle)

  val phyRegs = RegInit(VecInit(Seq.fill(pRegNum)(0.U(dataWidth.W))))

  //read
  (0 until issueNum).map(i => {
    val read = io.readPorts(i)
    asg(read(0).data, phyRegs(read(0).addr))
    asg(read(1).data, phyRegs(read(1).addr))
  })
  //write
  (0 until wBNum).map(i => {
    val write = io.writePorts(i)
    val wBits = write.bits
    //TODO:mask api
    val wdata = maskWord(wBits.result, wBits.wmask).asUInt | maskWord(phyRegs(wBits.pDest), ~wBits.wmask).asUInt
    when(write.valid && wBits.pDest =/= 0.U(pRegAddrWidth.W)) {
      phyRegs(wBits.pDest) := wdata
    }
  })

  def read(raddr: Vec[Vec[UInt]]) = {
    require(raddr.length == issueNum)
    (0 until issueNum).map(i => {
      asg(this.io.readPorts(i)(0).addr, raddr(i)(0))
      asg(this.io.readPorts(i)(1).addr, raddr(i)(1))
    })
    (0 until issueNum).map(i => { (0 until srcDataNum).map(j => this.io.readPorts(i)(j).data) })
  }
  //DiffTest ===================================================
  if (verilator) {
    import chisel3.util.experimental.BoringUtils._
    val pRegNumOfArchReg = Wire(Vec(aRegNum, PRegIdx))
    addSink(pRegNumOfArchReg, s"DiffArchRegNum")
    val checkArchRegs = Module(new DifftestArchIntRegState)
    checkArchRegs.io.clock := clock
    (0 until 32).foreach(i => {
      checkArchRegs.io.gpr(i) := phyRegs(pRegNumOfArchReg(i))
    })
    addSink(checkArchRegs.io.en, "hasValidRetire")
  }
}
