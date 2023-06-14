package utils

import config._
import bundle._
import chisel3._
import chisel3.util.DecoupledIO
import chisel3.util.Valid
import chisel3.util.RegEnable
import os.makeDir

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
  */

object PipelineVecConnect {
  def apply[T <: Data](
    gen:             T,
    left:            Vec[DecoupledIO[T]],
    right:           Vec[DecoupledIO[T]],
    rightOutFireNum: UInt
  ): Unit = {

    val size = left.length //also right.length

    //当拍  right的valid数
    val validNum = List.tabulate(size)(i => { right(i).valid }).foldRight(0.U)((sum, i) => sum.asUInt +& i)
    //下拍  right中原先valid  inst会留下（指还在slots中，但是所在槽可能会变）的数目
    val stayNum = validNum -& rightOutFireNum
    //下拍  给left的rdy数     =size-stayNum(其实right的接口并不需要提供ready
    val allowLeftNum = List.tabulate(size)(i => { right(i).ready }).foldRight(0.U)((sum, i) => sum.asUInt +& i)
    //situation: 1cango 2cantgo 3empty(rdy not rdy)
    List.tabulate(size)(i => {
      left(i).ready := (i.U < allowLeftNum)
    })

    //rightOutFireNum = first notFire slot's index
    List.tabulate(size)(i => {
      when(i.U < stayNum) {
        right(i).bits  := right(i.U + rightOutFireNum).bits
        right(i).valid := right(i.U + rightOutFireNum).valid
      }.otherwise {
        right(i).bits  := left(i.U - stayNum).bits
        right(i).valid := left(i.U - stayNum).valid
      }
    })

  }
}
