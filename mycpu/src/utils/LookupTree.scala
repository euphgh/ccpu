package utils

import chisel3._
import chisel3.util._

object LookupUInt {
  def apply[T <: Data](key: UInt, mapping: Iterable[(UInt, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))
}

object LookupUIntDefault {
  def apply[T <: Data](key: UInt, default: T, mapping: Iterable[(UInt, T)]): T =
    MuxLookup(key, default)(mapping.toSeq)
}

object LookupEnum {
  def apply[S <: EnumType, T <: Data](key: S, mapping: Iterable[(S, T)]): T =
    Mux1H(mapping.map(p => (p._1 === key, p._2)))
}

object LookupEnumDefault {
  def apply[S <: EnumType, T <: Data](key: S, default: T)(mapping: Seq[(S, T)]): T =
    MuxLookup(key.asUInt, default)(mapping.map { case (s, t) => (s.asUInt, t) })
}
