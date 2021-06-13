package tydal.schema

sealed trait Sort[+F <: Field[_]]:
  def field: F

case class Asc[+F <: Field[_]](field: F) extends Sort[F]
case class Desc[+F <: Field[_]](field: F) extends Sort[F]

trait SortByClasue[-X]

object SortByClasue:
  given [F <: Field[_]]: SortByClasue[F] with { }
  given [S <: Sort[_]]: SortByClasue[S] with { }
  given SortByClasue[EmptyTuple] with { }
  given [H, T <: Tuple](using SortByClasue[H], SortByClasue[T]): SortByClasue[H *: T] with { }
