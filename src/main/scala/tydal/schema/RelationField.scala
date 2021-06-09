package tydal.schema

final class RelationField[RelationAlias, Name, T](using val relationAlias: DbIdentifier[RelationAlias], val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = s"${relationAlias.value}.${name.value}"

trait RelationFields[RelationAlias, InputFields, OutputFields]:
  def value: OutputFields

object RelationFields:
  given emptyTuple[RelationAlias: DbIdentifier]: RelationFields[RelationAlias, EmptyTuple, EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given nonEmptyTuple[RelationAlias: DbIdentifier, HeadInput, HeadOutput, TailInput <: Tuple, TailOutput <: Tuple] (
    using
    head: RelationFields[RelationAlias, HeadInput, HeadOutput],
    tail: RelationFields[RelationAlias, TailInput, TailOutput]
  ): RelationFields[RelationAlias, HeadInput *: TailInput, HeadOutput *: TailOutput] with
    def value: HeadOutput *: TailOutput = head.value *: tail.value

  given column[RelationAlias: DbIdentifier, ColumnName: DbIdentifier, ColumnType: DbType]: RelationFields[RelationAlias, Column[ColumnName, ColumnType], RelationField[RelationAlias, ColumnName, ColumnType]] with
    def value: RelationField[RelationAlias, ColumnName, ColumnType] = RelationField[RelationAlias, ColumnName, ColumnType]

  given relationField[RelationAlias: DbIdentifier, PreviousRelationAlias, FieldAlias: DbIdentifier, FieldType: DbType]: RelationFields[RelationAlias, RelationField[PreviousRelationAlias, FieldAlias, FieldType], RelationField[RelationAlias, FieldAlias, FieldType]] with
    def value: RelationField[RelationAlias, FieldAlias, FieldType] = RelationField[RelationAlias, FieldAlias, FieldType]

  given tagged[RelationAlias: DbIdentifier, FieldAlias: DbIdentifier, FieldType: DbType, F <: Field[FieldType]]: RelationFields[RelationAlias, Tagged[F, FieldAlias], RelationField[RelationAlias, FieldAlias, FieldType]] with
    def value: RelationField[RelationAlias, FieldAlias, FieldType] = RelationField[RelationAlias, FieldAlias, FieldType]

  given cast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U: DbType](using base: RelationFields[RelationAlias, F, G]): RelationFields[RelationAlias, Cast[F, U], Cast[G, U]] with
    def value: Cast[G, U] = Cast(base.value)

  given softCast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U: DbType](using base: RelationFields[RelationAlias, F, G]): RelationFields[RelationAlias, SoftCast[F, U], SoftCast[G, U]] with
    def value: SoftCast[G, U] = SoftCast(base.value)
