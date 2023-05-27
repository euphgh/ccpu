package config

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait MycpuParam {
  // General Parameter for mycpu

  val excCodeWidth = 5
  //val PaddrWidth = TODO:
  val paddrTagWidth   = 11 //TODO:
  val cacheIndexWidth = 12
  val vaddrWidth      = 32
  val instrWidth      = 32
  val dataWidth       = 32

  val predictNum = 4
  val fetchNum   = 4
  val decodeNum  = 2
  val renameNum  = 2
  val wBNum      = 3
  val issueNum   = 2 //should be 4
  val srcDataNum = 2
  val retireNum  = 2 //should be 4

  val aRegNum       = 32
  val aRegAddrWidth = log2Up(aRegNum)
  val pRegNum       = 64
  val pRegAddrWidth = log2Up(pRegNum)
  val robNum        = 32
  val robIndexWidth = log2Up(robNum)

  val prfReadPortNum = srcDataNum * issueNum

  val aluFuNum = 2

  val forAlu = 0
  val forMdu = 1
  val forLsu = 2

  val aluExternBypassNum = 1
}

abstract class MycpuBundle extends Bundle with MycpuParam
