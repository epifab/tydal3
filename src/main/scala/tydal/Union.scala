package tydal

final class Union[FieldsA, +A <: SelectLike[FieldsA], FieldsB, +B <: SelectLike[FieldsB]](val a: A, val b: B, val distinct: Boolean)(using UnifiableFields[FieldsA, FieldsB]) extends SelectLike[FieldsA]:
  val fields = a.fields

trait UnifiableFields[-A, -B]

object UnifiableFields:
  given empty: UnifiableFields[EmptyTuple, EmptyTuple] with { }

  given nonEmpty[AH, BH, H, AT <: Tuple, BT <: Tuple, T <: Tuple] (
    using
    head: UnifiableFields[AH, BH],
    tail: UnifiableFields[AT, BT]
  ): UnifiableFields[AH *: AT, BH *: BT] with { }

  given field[T, F <: Field[T], G <: Field[T]]: UnifiableFields[F, G] with { }
