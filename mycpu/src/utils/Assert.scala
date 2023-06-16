package utils
import chisel3._
import chisel3.util._

object asg {
  def apply[T <: Data](left: T, right: T) = {
    require(left.getWidth == right.getWidth)
    left := right
  }
}

object vassert {
  def apply(cond: Bool) = {}
}
