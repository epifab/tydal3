package tydal.schema

trait ListOfFields[-Fields]

object ListOfFields:
  given [F <: Field[_]]: ListOfFields[F] with { }
  given ListOfFields[EmptyTuple] with { }
  given [H, T <: Tuple](using ListOfFields[H], ListOfFields[T]): ListOfFields[H *: T] with { }


trait NonEmptyListOfFields[-Fields]

object NonEmptyListOfFields:
  given [F <: Field[_]]: NonEmptyListOfFields[F] with { }
  given twoOrMore[H, T <: NonEmptyTuple](using NonEmptyListOfFields[H], NonEmptyListOfFields[T]): NonEmptyListOfFields[H *: T] with { }
  given one[H](using ListOfFields[H]): NonEmptyListOfFields[H *: EmptyTuple] with { }


trait OptionalPlaceholder[Type, -X]

object OptionalPlaceholder:
  given none[Type]: OptionalPlaceholder[Type, None.type] with { }
  given some[Name, Type]: OptionalPlaceholder[Type, Some[Placeholder[Name, Type]]] with { }

type OptionalInt4[-X] = OptionalPlaceholder[int4, X]
type OptionalInt8[-X] = OptionalPlaceholder[int8, X]
