package utils

import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Valid
import chisel3.util.RegEnable

/**
  * 4 kinds of connect:
  *    normal pipeline:single issue(preIF-IF1-IF2)(RS-RO-EXE/MEM)
  *    ibf to dper
  *    write into buffer:(IF2-IBF)(DPER-ROB/RS)treat as wen
  *    WB rob/prf:TODO:arbiter
  */

//preIF-IF1-IF2
object PipelineConnect {
  def apply[T <: Data](left: DecoupledIO[T], right: DecoupledIO[T], rightOutFire: Bool, isFlush: Bool) = {
    val valid = RegInit(false.B)
    when(rightOutFire) { valid := false.B }
    when(right.ready) { valid := left.valid }
    when(isFlush) { valid := false.B }

    left.ready  := right.ready
    right.bits  := RegEnable(left.bits, left.valid && right.ready)
    right.valid := valid //attention:here right.valid means "pipex_valid"
  }
}

/**
  * dad:left.out
  * bro:right(older).in
  *
  * inRight = 从right和left中选
  *     right：关注staynum
  *     left：除了staynum之外的
  * right每一拍都用inRight“更新”
  *     这里着重关注right.valid,在dp.scala中它以线的方式直接赋值给slot.valid
  *
  * allowLeftNum由right产生：用于生成left.out.rdy
  *
  * flush包括exception/eret/mispredict
  */

object IbfConnectDper {
  def apply[T <: Data](
    gen:             T,
    left:            Vec[DecoupledIO[T]],
    right:           Vec[Valid[T]],
    rightOutFireNum: UInt,
    flush:           Bool
  ): Unit = {

    val size = left.length //also right.length

    //当拍  right的valid数
    val validNum = List.tabulate(size)(i => { right(i).valid }).foldRight(0.U)((sum, i) => sum.asUInt +& i)
    //下拍  right中原先valid  inst会留下（指还在slots中，但是所在槽可能会变）的数目
    val stayNum = validNum -& rightOutFireNum
    //下拍  给left的rdy数
    val allowLeftNum = size.U -& stayNum
    //situation: 1cango 2cantgo 3empty(rdy not rdy)
    List.tabulate(size)(i => {
      left(i).ready := (i.U < allowLeftNum)
    })

    //rightOutFireNum = first notFire slot's index
    val inRight = Reg(Vec(size, Valid(gen)))
    List.tabulate(size)(i => {
      when(i.U < stayNum) {
        inRight(i).bits  := right(i.U + rightOutFireNum).bits
        inRight(i).valid := right(i.U + rightOutFireNum).valid
      }.otherwise {
        inRight(i).bits  := left(i.U - stayNum).bits
        inRight(i).valid := left(i.U - stayNum).valid
      }
    })

    when(flush) { (0 until size).map(i => inRight(i).valid := false.B) }
    (0 until size).map(i => right(i) := inRight(i))

  }
}
