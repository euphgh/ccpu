package utils
import chisel3._
import chisel3.util._

object asg {
  def apply[T <: Data](left: T, right: T) = {
    require(
      left.getWidth == right.getWidth,
      s"left width(${left.getWidth}) is not equal with right width(${right.getWidth})"
    )
    left := right
  }
  def apply[T <: Data](left: Vec[T], right: Vec[T]) = {
    require(left.size == right.size)
    left := right
  }
}
