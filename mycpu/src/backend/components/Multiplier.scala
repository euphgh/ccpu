package backend.components
import chisel3.util.experimental.BoringUtils._
import config._
import backend._
import chisel3._
import chisel3.util._
import utils._
class MultiplierIP extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val rst = Input(Reset())
    val A   = Input(UInt(33.W))
    val B   = Input(UInt(33.W))
    val P   = Output(UInt(66.W))
  })
  setInline(
    "MultiplierIP",
    """ module MultiplierIP (
      |    input wire clk,rst,
      |    input wire [32:0] A,
      |    input wire [32:0] B,
      |    output wire [65:0] P
      |);
      |    
      |`ifndef VERILATOR
      |Multiplier Multiplier_u(
      |    /*autoinst*/
      |    .CLK                    (clk), //input */
      |    .A                      (A), //input */
      |    .B                      (B), //input */
      |    .P                      (P)  //output */
      |); 
      |`else
      |    reg [65:0] seg1,seg0;
      |    always @(posedge clk) begin
      |        if (!rst) begin
      |            seg0 <= 0;
      |            seg1 <= 0;
      |        end
      |        else begin
      |            seg0 <= {{33{A[32]}}, A} * {{33{B[32]}}, B};	// MyMultipler.scala:9:{20,34}
      |            seg1 <= seg0;
      |        end
      |    end
      |    assign P = seg1;
      |`endif
      |
      |endmodule
    """.stripMargin
  )
}

class MulComponent extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
      val isAdd  = Input(Bool())
      val isSub  = Input(Bool())
    }))
    val out   = Valid(Output(UInt(64.W)))
    val flush = Input(Bool())
  })

  val inBits = io.in.bits
  val op     = Wire(Vec(srcDataNum, UInt(33.W)))
  (0 until srcDataNum).foreach(i => {
    asg(op(i), Cat(inBits.isSign && inBits.srcs(i)(31), inBits.srcs(i)))
  })
  val add = inBits.isAdd
  val sub = inBits.isSub
  val ip  = Module(new MultiplierIP)
  ip.io.clk := this.clock
  ip.io.rst := !this.reset.asBool
  asg(ip.io.A, op(0))
  asg(ip.io.B, op(1))
  val wirehi  = Wire(UWord)
  val wirelo  = Wire(UWord)
  val mres    = WireInit(0.U(64.W)) //init
  val mresReg = Reg(UInt(64.W))
  mresReg := mres
  addSink(wirehi, "specHIdata")
  addSink(wirelo, "specLOdata")
  io.out.valid := false.B
  val run :: mul :: addsub :: finish :: Nil = Enum(4)
  // state
  val state = RegInit(run)
  assert(~(io.out.valid & state =/= finish))
  switch(state) {
    is(run) {
      state := Mux(io.flush, run, Mux(io.in.valid, mul, run))
    }
    is(mul) {
      state := Mux(io.flush, run, Mux(add || sub, addsub, finish))
    }
    is(finish) {
      state        := run
      io.out.valid := !io.flush
    }
    is(addsub) {
      state := Mux(io.flush, run, finish)
      val uOp  = List(Cat(wirehi, wirelo), ip.io.P(63, 0))
      val sOp  = uOp.map(_.asSInt)
      val ures = Wire(UInt(64.W))
      val sres = Wire(SInt(64.W))
      asg(ures, Mux(add, uOp(0) + uOp(1), uOp(0) - uOp(1)))
      asg(sres, Mux(add, sOp(0) + sOp(1), sOp(0) - sOp(1)))
      mres := Mux(inBits.isSign, sres.asUInt, ures)
    }
  }
  asg(io.out.bits, Mux(inBits.isAdd || inBits.isSub, mresReg, ip.io.P(63, 0)))
}
