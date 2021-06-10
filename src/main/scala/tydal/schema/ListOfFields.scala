package tydal.schema

trait ListOfFields[-Fields]

object ListOfFields:
  given [F <: Field[_]]: ListOfFields[F] with { }
  given [F <: Field[_], T]: ListOfFields[Tagged[F, T]] with { }
  given ListOfFields[EmptyTuple] with { }
  given [H, T <: Tuple](using ListOfFields[H], ListOfFields[T]): ListOfFields[H *: T] with { }
