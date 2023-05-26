import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

trait MycpuParam {
  // General Parameter for mycpu
  //val PaddrWidth = TODO:
  val PaddrTagWidth   = 11 //TODO:
  val cacheIndexWidth = 12
  val VaddrWidth      = 32
  val instrWidth      = 32
  val dataWidth       = 32

  val PredictNum = 4
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
}

abstract class MycpuBundle extends Bundle with MycpuParam
