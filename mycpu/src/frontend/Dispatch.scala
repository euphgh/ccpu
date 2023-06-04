/* package frontend
import config._
import bundle._
import chisel3._
import chisel3.util.Decoupled
import chisel3.util.Valid
class Dispatcher extends MycpuModule {
  val io = IO(new Bundle {
    val in = new Bundle {
      val fromInstBuffer  = Vec(decodeNum, Flipped(Decoupled(new InstBufferOutIO)))
      val fromFuWriteBack = Vec(retireNum, Flipped(Valid(new SRATWriteBackIO))) //后端那组合一下
    }
    //to RS 和to rob有些重复了，改掉吧
    //TODO:觉得Rs的那玩意儿还是保留，然后这里引出去的就是一个完整的东西，在后端再分配到各个保留站
    val toRs = Vec(
      renameNum,
      Decoupled(new Bundle {
        val basic         = new RsBasicEntry
        val predictResult = new PredictResultBundle
      })
    )
    val rob = Vec(
      renameNum,
      Decoupled(new Bundle {
        val out = new DispatchToRobBundle //这里可以把端口写在外面
        val in  = ROBIdx //这个不需要
      })
    )
  })
} */
