package difftest

import chisel3._
import chisel3.util._
import chisel3.reflect._
import chisel3.experimental.ExtModule
import config.MycpuParam

trait DifftestWithClock {
  val clock = Input(Clock())
  val en    = Input(Bool())
}

abstract class DifftestBundle extends Bundle with DifftestWithClock

class DiffInstrCommitIO extends DifftestBundle {
  val retireNum = Input(UInt(8.W))
  val lastPC    = Input(UInt(32.W))
  val interrSeq = Input(UInt(8.W))
}

class DiffArchIntRegStateIO extends DifftestBundle {
  val gpr = Input(Vec(32, UInt(32.W)))
}

class DiffPhyRegInROBIO extends DifftestBundle with MycpuParam {
  val robHead   = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val robTail   = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val rob       = Input(Vec(robNum, PRegIdx))
  val flrHead   = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val flrTail   = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val flr       = Input(Vec(robNum, PRegIdx))
  val isRecover = Input(Bool())
}

class DiffPhyRegInFreeListIO extends DifftestBundle with MycpuParam {
  val flHead = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val flTail = Input(UInt((log2Ceil(freeListSize) + 1).W))
  val fl     = Input(Vec(freeListSize, PRegIdx))
}

class DiffArchHiloIO extends DifftestBundle {
  val hi = Input(UInt(32.W))
  val lo = Input(UInt(32.W))
}

class DiffArchCopIO extends DifftestBundle {
  val index    = Input(UInt(32.W))
  val random   = Input(UInt(32.W))
  val entrylo0 = Input(UInt(32.W))
  val entrylo1 = Input(UInt(32.W))
  val context  = Input(UInt(32.W))
  val pagemask = Input(UInt(32.W))
  val wire     = Input(UInt(32.W))
  val badvaddr = Input(UInt(32.W))
  val count    = Input(UInt(32.W))
  val entryhi  = Input(UInt(32.W))
  val compare  = Input(UInt(32.W))
  val status   = Input(UInt(32.W))
  val cause    = Input(UInt(32.W))
  val epc      = Input(UInt(32.W))
  val prid     = Input(UInt(32.W))
  val ebase    = Input(UInt(32.W))
  val config0  = Input(UInt(32.W))
  val config1  = Input(UInt(32.W))
}

class DiffDCacheIO extends DifftestBundle {
  val mainState = Input(UInt(4.W))
  val hasValid  = Input(Bool())

  val reqAddr   = Input(UInt(32.W))
  val isUncache = Input(Bool())
  val isWrite   = Input(Bool())
  val writeData = Input(UInt(32.W))
  val cancel    = Input(Bool())

  val retData    = Input(UInt(32.W))
  val hitWays    = Input(UInt(2.W))
  val isHit      = Input(Bool())
  val victimWay  = Input(UInt(2.W))
  val vicTag     = Input(UInt(32.W))
  val vicDirty   = Input(Bool())
  val vicValid   = Input(Bool())
  val vicLine    = Input(Input(Vec(8, UInt(32.W))))
  val writeState = Input(UInt(4.W))

  val instrOp     = Input(UInt(5.W))
  val instrOk     = Input(Bool())
  val instrRetire = Input(Bool())
}

abstract class DifftestModule[T <: DifftestBundle] extends ExtModule with HasExtModuleInline {
  val io: T

  def getDirectionString(data: Data): String = {
    if (DataMirror.directionOf(data) == ActualDirection.Input) "input " else "output"
  }

  def getDPICArgString(argName: String, data: Data, size: Int): String = {
    val directionString = getDirectionString(data)
    val typeString = (data.getWidth / size) match {
      case 1                                  => "bit"
      case width if width > 1 && width <= 8   => "byte"
      case width if width > 8 && width <= 32  => "int"
      case width if width > 32 && width <= 64 => "longint"
      case _                                  => s"unsupported io type of width ${data.getWidth}!!\n"
    }
    val array     = if (size > 1) s"[$size]" else ""
    val argString = Seq(directionString, f"${typeString}%7s", argName, array)
    argString.mkString(" ")
  }

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    val argString   = Seq(getDirectionString(data), widthString, s"$argName")
    argString.mkString(" ")
  }

  def getVecCode(argName: String, data: Data, size: Int) = {
    val dataWidth = data.getWidth / size
    val typeString = dataWidth match {
      case 1                                  => "bit"
      case width if width > 1 && width <= 8   => "byte"
      case width if width > 8 && width <= 32  => "int"
      case width if width > 32 && width <= 64 => "longint"
      case _                                  => s"unsupported io type of width ${data.getWidth}!!\n"
    }
    val code = new StringBuilder(s"wire $typeString $argName [$size];\n")
    (0 until size).foreach(i => {
      code.append(s"assign ${argName}[$i] = ${argName}_$i;\n")
    })
    code.toString()
  }
  def moduleName = this.getClass.getSimpleName
  def moduleBody: String = {
    // ExtModule implicitly adds io_* prefix to the IOs (because the IO val is named as io).
    val interfaces = io.elements.toSeq.reverse.flatMap {
      case (name, data) =>
        data match {
          case vec: Vec[_] => vec.zipWithIndex.map { case (v, i) => (s"io_${name}_$i", v) }
          case _ => Seq((s"io_$name", data))
        }
    }
    val dpiInterfaces = io.elements.toSeq.reverse.flatMap {
      case (name, data) =>
        data match {
          case vec: Vec[_] => Seq((s"io_$name", data, vec.size))
          case _ => Seq((s"io_$name", data, 1))
        }
    }
    // (1) DPI-C function prototype
    val dpicName = s"v_difftest_${moduleName.replace("Difftest", "")}"
    val dpicDecl =
      s"""
         |import "DPI-C" function void $dpicName (
         |${dpiInterfaces
        .filterNot(in => in._1 == "io_clock" || in._1 == "io_en")
        .map(ifc => getDPICArgString(ifc._1, ifc._2, ifc._3))
        .mkString(",\n")}
         |);
         |""".stripMargin
    // (2) module definition
    val modPorts = interfaces.map(i => getModArgString(i._1, i._2)).mkString(",\n")
    // (3) vec init
    val argToVec = dpiInterfaces.filter(_._3 > 1).map(i => getVecCode(i._1, i._2, i._3))
    // (4) func call
    val funcCallStr = dpiInterfaces
      .map(_._1)
      .filterNot(in => in == "io_clock" || in == "io_en")
    val modDef =
      s"""
         |module $moduleName(
         |  $modPorts
         |);
         |`ifndef SYNTHESIS
         |`ifdef VERILATOR
         |$dpicDecl
         |${argToVec.mkString}
         |  always @(posedge io_clock) begin
         |    if (io_en) begin
         |      $dpicName (${funcCallStr.mkString(",")});
         |    end 
         |  end
         |`endif
         |`endif
         |endmodule
         |""".stripMargin
    modDef
  }
  def instantiate(): Unit = setInline(s"$moduleName.v", moduleBody)
}

class DifftestBaseModule[T <: DifftestBundle](gen: T) extends DifftestModule[T] {
  val io = IO(gen)
  instantiate()
}

class DifftestInstrCommit extends DifftestBaseModule(new DiffInstrCommitIO)
class DifftestArchHILO extends DifftestBaseModule(new DiffArchHiloIO)
class DifftestArchCP0 extends DifftestBaseModule(new DiffArchCopIO)
class DifftestArchIntRegState extends DifftestBaseModule(new DiffArchIntRegStateIO)
class DifftestPhyRegInROB extends DifftestBaseModule(new DiffPhyRegInROBIO)
class DifftestPhyRegInFreeList extends DifftestBaseModule(new DiffPhyRegInFreeListIO)
class DifftestCacheRun extends DifftestBaseModule(new DiffDCacheIO)
