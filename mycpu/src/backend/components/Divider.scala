package backend.components

import config._
import backend._
import chisel3._
import chisel3.util._
import utils._
import chisel3.experimental.ExtModule
class Divider extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
    }))
    val out   = Valid(UInt(64.W))
    val flush = Input(Bool())
  })
  val srcs          = io.in.bits.srcs
  val isSign        = io.in.bits.isSign
  val signQ, signR  = RegInit(false.B)
  val cnt           = Counter(16)
  val x, y1, y2, y3 = RegInit(0.U(64.W))
  val quot          = RegInit(0.U(32.W))
  val y1_init       = Cat(0.U(2.W), Mux(srcs(1)(31) && isSign, ~srcs(1) + 1.U, srcs(1)), 0.U(30.W))
  val sub1          = x -& y1
  val sub2          = x -& y2
  val sub3          = x -& y3
  val quotient      = Wire(UWord)
  val reminder      = Wire(UWord)

  val idle :: work :: finish :: Nil = Enum(3)
  // state
  val state = RegInit(idle)
  assert(~(io.out.valid & state =/= finish))
  io.out.valid := false.B
  switch(state) {
    is(idle) {
      asg(x, Cat(0.U(32.W), Mux(srcs(0)(31) && isSign, ~srcs(0) + 1.U, srcs(0))))
      asg(y1, y1_init)
      asg(y2, (y1_init << 1)(63, 0))
      asg(y3, y1_init + (y1_init << 1)(63, 0))
      asg(signQ, (srcs(0)(31) ^ srcs(1)(31)) && isSign)
      asg(signR, srcs(0)(31) && isSign)
      cnt.reset()
      state := Mux(io.in.valid, work, idle)
      when(io.in.valid) {
        // printf(s"req Signal: %b\n", isSign)
        // printf(s"signQ: %b\n", signQ)
        // printf(s"signR: %b\n", signR)
      }
    }
    is(work) {
      asg(
        x,
        MuxCase(
          x,
          Seq(
            !sub3(64) -> sub3(63, 0),
            !sub2(64) -> sub2(63, 0),
            !sub1(64) -> sub1(63, 0)
          )
        )
      )
      y1 := y1 >> 2
      y2 := y2 >> 2
      y3 := y3 >> 2
      asg(quot, (quot << 2)(31, 0) | Cat(0.U(30.W), Cat(!sub3(64) || !sub2(64), !sub3(64) || (sub2(64) && !sub1(64)))))
      val wrap = cnt.inc()
      state := Mux(wrap, finish, work)
      // printf(s"work count :%d\n", cnt.value)
      // printf(s"quotient :%x\n", quot)
      // printf(s"reminder :%x\n", x)
    }
    is(finish) {
      io.out.valid := true.B
      state        := idle
      // printf(s"finish count :%d\n", cnt.value)
      // printf(s"quotient :%x\n", quotient)
      // printf(s"reminder :%x\n", reminder)
    }
  }
  when(io.flush) {
    state := idle
  }
  asg(quotient, Mux(signQ, ~quot + 1.U, quot))
  val xLow32 = x(31, 0)
  asg(reminder, Mux(signR, ~xLow32 + 1.U, xLow32))
  asg(io.out.bits, Cat(reminder, quotient))
}

class DividerIP32 extends ExtModule with HasExtModuleInline with MycpuParam {
  val clk        = IO(Input(Clock()))
  val rst        = IO(Input(Reset()))
  val div        = IO(Input(Bool()))
  val div_signed = IO(Input(Bool()))
  val x, y       = IO(Input(UWord))
  val div_tready = IO(Output(Bool()))
  val s, r       = IO(Output(UWord))
  val complete   = IO(Output(Bool()))
  val timer_out  = IO(Output(UInt(6.W)))
  setInline(
    "DividerIP.sv",
    """
module DividerIP (
    input   wire        clk,
    input	  wire        rst,
    input   wire        div,//tvalid
    input   wire        div_signed, //{0:无符号,1:有符号}
    input   wire        [31:0] x,y,
    output  wire        div_tready,
    output  wire        [31:0] s,r,
    output  wire        complete,
    output  wire        [5:0] timer_out
);
/*====================Variable Declaration====================*/
wire x_sign,y_sign;
wire first;
wire [31:0] x_abs,y_abs;
reg [5:0] timer; 
reg [31:0] divisor,quotient_iter;
reg [63:0] minuend;
wire reminder_sign,quotient_sign;
reg reminder_sign_r,quotient_sign_r;
wire  [31:0]  quotient_temp;
wire  [63:0]  minuend_back;
wire pre_complete;
reg  have_data;
reg [31:0] quotient_temp_r;
reg [31:0] minuend_back_r;
reg pre_complete_r;
/*====================Function Code====================*/
assign div_tready = div&&(pre_complete_r||(!have_data));
assign x_sign = x[31]&&div_signed;
assign y_sign = y[31]&&div_signed;
assign x_abs = ({32{x_sign}}^x) + x_sign;
assign y_abs = ({32{y_sign}}^y) + y_sign; 
assign first = !(|timer);
assign quotient_sign = (x[31]^y[31]) && div_signed;
assign reminder_sign = x[31] && div_signed;

always @(posedge clk ) begin
    if (!(rst)||pre_complete_r) begin
        divisor <= 32'hffff_ffff;
        minuend <= 64'b0;
        timer <= 6'b0;
        quotient_iter <= 32'b0;
        reminder_sign_r <= 1'b0;
        quotient_sign_r <=1'b0;
        have_data <= 1'b0;
    end
    else if (div_tready) begin
        timer <= 1'b1;
        minuend <= {32'b0,x_abs};
        divisor <= y_abs;
        quotient_iter <= 32'b0; 
        reminder_sign_r <= reminder_sign;
        quotient_sign_r <= quotient_sign;
        have_data <= 1'b1;
    end
    else if (have_data) begin
        timer <= timer + 1'b1;
        minuend <= minuend_back;
        quotient_iter <= quotient_temp;
    end
end

wire [32:0] diff;
wire [63:0] minuend_new;

assign diff = minuend[63:31] - {1'b0,divisor};
assign minuend_new = {diff,minuend[30:0]};
assign quotient_temp = (quotient_iter<<1) + (!diff[32]);
assign minuend_back = (diff[32] ? minuend : minuend_new)<<1;
assign pre_complete = (timer==6'd32);

always @(posedge clk ) begin
    if (!(rst)||pre_complete_r) begin
        quotient_temp_r <= 32'b0;
        minuend_back_r <= 32'b0;
        pre_complete_r <= 1'b0;
    end
    else begin
        quotient_temp_r <= quotient_temp;
        minuend_back_r <= minuend_back[63:32];
        pre_complete_r <= pre_complete;
    end
end
assign s = quotient_sign_r ? (~quotient_temp_r+1'b1) : quotient_temp_r;
assign r = reminder_sign_r ? (~minuend_back_r+1'b1) : minuend_back_r;
assign complete = pre_complete_r || (!have_data);
assign timer_out = timer;
endmodule
  """.stripMargin
  )
}

class DividerIP16 extends ExtModule with HasExtModuleInline with MycpuParam {
  val clk    = IO(Input(Clock()))
  val resetn = IO(Input(Reset()))
  val en     = IO(Input(Bool()))
  val sign   = IO(Input(Bool()))
  val A, B   = IO(Input(UWord))

  val Q, R    = IO(Output(UWord))
  val working = IO(Output(Bool()))
  val finish  = IO(Output(Bool()))
  setInline(
    "DividerIP16.v",
    """
      |`timescale 1ns/100ps
      |
      |// * refer to ucas div
      |module DividerIP16(
      |    input   clk,
      |    input   resetn,
      |    
      |    input   en,         // * 除法使能, 1: 有除法请求, 0: 无
      |    input   sign,       // * 1: signed, 0: else
      |    input [31:0]    A,
      |    input [31:0]    B,
      |
      |    output [31:0]   Q,  // * Quotient
      |    output [31:0]   R,  // * remainder
      |
      |    output          working,
      |    output          finish
      |);
      |
      |    reg [4:0] cnt;
      |    reg sign_Q, sign_R;  
      |    reg [63:0] x, y1, y2, y3;
      |    reg [31:0] quot;
      |    wire [63:0] y1_init = {2'd0, (B[31] && sign) ? ~B+1'b1 : B, 30'd0};
      |    wire [64:0] sub1 = x - y1;
      |    wire [64:0] sub2 = x - y2;
      |    wire [64:0] sub3 = x - y3;
      |
      |    always @(posedge clk) begin
      |        if(!resetn)             cnt <= 'd0;
      |        else if(en)             cnt <= 'd1;
      |        else if(finish)         cnt <= 'd0;
      |        else if(working)        cnt <= cnt+'d1;
      |    end
      |
      |    always @(posedge clk) begin
      |        if(!resetn) begin
      |            x   <= 64'd0;
      |            y1  <= 64'd0;
      |            y2  <= 64'd0;
      |            y3  <= 64'd0;
      |            quot<= 32'd0;
      |            sign_Q  <= 1'b0;
      |            sign_R  <= 1'b0;
      |        end
      |        else if(en) begin
      |            x   <= {32'd0, A[31] && sign ? ~A+1'b1 : A};
      |            y1  <= y1_init;
      |            y2  <= y1_init << 1;
      |            y3  <= y1_init + (y1_init << 1);
      |            sign_Q  <= (A[31]^B[31]) && sign;
      |            sign_R  <= A[31] && sign;
      |        end
      |        else if(cnt != 5'd17) begin
      |            x   <=  !sub3[64] ? sub3[63:0] :
      |                    !sub2[64] ? sub2[63:0] :
      |                    !sub1[64] ? sub1[63:0] : x;
      |            y1  <= y1 >> 2;
      |            y2  <= y2 >> 2;
      |            y3  <= y3 >> 2;
      |            quot<= (quot << 2) | {30'd0, {!sub3[64] || !sub2[64], !sub3[64] || (sub2[64] && !sub1[64])}};
      |        end
      |    end
      |
      |    assign Q    = {32{sign_Q}}  & (~quot+1'b1) | {32{!sign_Q}} & quot;
      |    assign R    = {32{sign_R}}  & (~x[31:0]+1'b1) | {32{!sign_R}} & x[31:0];
      |    assign working = cnt != 5'd0 && !finish;
      |    assign finish = cnt == 5'd17;
      |
      |endmodule
  """.stripMargin
  )

}

class DivierTester32 extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
    }))
    val out = Valid(new Bundle {
      val quotient = UInt(32.W)
      val reminder = UInt(32.W)
    })
  })
  val ip     = Module(new DividerIP32)
  val inBits = io.in.bits
  ip.clk               := clock
  ip.rst               := reset
  ip.div               := io.in.valid
  ip.div_signed        := inBits.isSign
  ip.x                 := inBits.srcs(0)
  ip.y                 := inBits.srcs(1)
  io.out.valid         := ip.div_tready
  io.out.bits.quotient := ip.s
  io.out.bits.reminder := ip.r
}

class DivierTest16 extends MycpuModule {
  val io = IO(new Bundle {
    val in = Flipped(Valid(new Bundle {
      val srcs   = Vec(srcDataNum, Output(UInt(dataWidth.W)))
      val isSign = Input(Bool())
    }))
    val out = Valid(new Bundle {
      val quotient = UInt(32.W)
      val reminder = UInt(32.W)
    })
  })
  val ip     = Module(new DividerIP16)
  val inBits = io.in.bits
  ip.clk               := clock
  ip.resetn            := reset
  ip.en                := io.in.valid
  ip.sign              := inBits.isSign
  ip.A                 := inBits.srcs(0)
  ip.B                 := inBits.srcs(1)
  io.out.valid         := ip.finish
  io.out.bits.quotient := ip.Q
  io.out.bits.reminder := ip.R
}
