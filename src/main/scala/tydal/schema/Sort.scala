package tydal.schema

sealed trait Sort

case class Asc[F <: Field[_]](field: F) extends Sort
case class Desc[F <: Field[_]](field: F) extends Sort
