package tydal

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


trait OptionalInput[Type, -X]

object OptionalInput:
  given none[Type]: OptionalInput[Type, None.type] with { }
  given somePlaceholder[Name, Type]: OptionalInput[Type, Some[Placeholder[Name, Type]]] with { }
  given someConst[Type]: OptionalInput[Type, Some[Const[Type]]] with { }

type OptionalInt4[-X] = OptionalInput[int4, X]
type OptionalInt8[-X] = OptionalInput[int8, X]
