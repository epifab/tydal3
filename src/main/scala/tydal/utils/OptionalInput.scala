package tydal
package utils

/**
 * Given Type, it ensures that X is either None, Some[Placeholder[*, Type]] or Some[Const[Type]]
 * @tparam Type
 * @tparam X
 */
trait OptionalInput[Type, -X]

object OptionalInput:
  given none[Type]: OptionalInput[Type, None.type] with { }
  given somePlaceholder[Name, Type]: OptionalInput[Type, Some[Placeholder[Name, Type]]] with { }
  given someConst[Type]: OptionalInput[Type, Some[Const[Type]]] with { }

type OptionalInt4[-X] = OptionalInput[int4, X]
type OptionalInt8[-X] = OptionalInput[int8, X]
