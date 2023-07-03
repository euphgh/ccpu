package difftest

import chisel3._
import chisel3.util._
import chisel3.reflect._
import chisel3.experimental.ExtModule

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

abstract class DifftestModule[T <: DifftestBundle] extends ExtModule with HasExtModuleInline {
  val io: T

  def getDirectionString(data: Data): String = {
    if (DataMirror.directionOf(data) == ActualDirection.Input) "input " else "output"
  }

  def getDPICArgString(argName: String, data: Data): String = {
    val directionString = getDirectionString(data)
    val typeString = data.getWidth match {
      case 1                                  => "bit"
      case width if width > 1 && width <= 8   => "byte"
      case width if width > 8 && width <= 32  => "int"
      case width if width > 32 && width <= 64 => "longint"
      case _                                  => s"unsupported io type of width ${data.getWidth}!!\n"
    }
    val argString = Seq(directionString, f"${typeString}%7s", argName)
    argString.mkString(" ")
  }

  def getModArgString(argName: String, data: Data): String = {
    val widthString = if (data.getWidth == 1) "      " else f"[${data.getWidth - 1}%2d:0]"
    val argString   = Seq(getDirectionString(data), widthString, s"$argName")
    argString.mkString(" ")
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
    // (1) DPI-C function prototype
    val dpicInterfaces = interfaces.filterNot(in => in._1 == "io_clock" || in._1 == "io_en")
    val dpicName       = s"v_difftest_${moduleName.replace("Difftest", "")}"
    val dpicDecl =
      s"""
         |import "DPI-C" function void $dpicName (
         |${dpicInterfaces.map(ifc => getDPICArgString(ifc._1, ifc._2)).mkString(",\n")}
         |);
         |""".stripMargin
    // (2) module definition
    val modPorts = interfaces.map(i => getModArgString(i._1, i._2)).mkString(",\n")
    val modDef =
      s"""
         |module $moduleName(
         |  $modPorts
         |);
         |`ifndef SYNTHESIS
         |`ifdef VERILATOR
         |$dpicDecl
         |  always @(posedge io_clock) begin
         |    if (io_en) begin
         |      $dpicName (${dpicInterfaces.map(_._1).mkString(",")});
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

class DifftestTop extends Module {
  var difftest_instr_commit     = Module(new DifftestInstrCommit);
  var difftest_arch_hilo        = Module(new DifftestArchHILO);
  var difftest_arch_cp0         = Module(new DifftestArchCP0);
  var difftest_arch_intregstate = Module(new DifftestArchIntRegState);
}
