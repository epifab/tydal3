package tydal
package utils

/**
 * Given an alias (RelationAlias) and either a single field or a tuple made of a list of fields (InputFields),
 * it builds either a single or a tuple made of RelationField
 *
 * @tparam RelationAlias
 * @tparam RelationFields
 * @tparam OutputFields
 */
trait RelationFieldsFactory[RelationAlias, RelationFields, OutputFields]:
  def value: OutputFields

object RelationFieldsFactory:
  given emptyTuple[RelationAlias: DbIdentifier]: RelationFieldsFactory[RelationAlias, EmptyTuple, EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given nonEmptyTuple[RelationAlias: DbIdentifier, HeadInput, HeadOutput, TailInput <: Tuple, TailOutput <: Tuple] (
    using
    head: RelationFieldsFactory[RelationAlias, HeadInput, HeadOutput],
    tail: RelationFieldsFactory[RelationAlias, TailInput, TailOutput]
  ): RelationFieldsFactory[RelationAlias, HeadInput *: TailInput, HeadOutput *: TailOutput] with
    def value: HeadOutput *: TailOutput = head.value *: tail.value

  given column[RelationAlias: DbIdentifier, ColumnName: DbIdentifier, ColumnType: DbType]: RelationFieldsFactory[RelationAlias, Column[ColumnName, ColumnType], RelationField[RelationAlias, ColumnName, ColumnType]] with
    def value: RelationField[RelationAlias, ColumnName, ColumnType] = RelationField[RelationAlias, ColumnName, ColumnType]

  given relationField[RelationAlias: DbIdentifier, PreviousRelationAlias, FieldAlias: DbIdentifier, FieldType: DbType]: RelationFieldsFactory[RelationAlias, RelationField[PreviousRelationAlias, FieldAlias, FieldType], RelationField[RelationAlias, FieldAlias, FieldType]] with
    def value: RelationField[RelationAlias, FieldAlias, FieldType] = RelationField[RelationAlias, FieldAlias, FieldType]

  given aliased[RelationAlias: DbIdentifier, FieldAlias: DbIdentifier, FieldType: DbType, F <: Field[FieldType]]: RelationFieldsFactory[RelationAlias, Aliased[FieldType, F, FieldAlias], RelationField[RelationAlias, FieldAlias, FieldType]] with
    def value: RelationField[RelationAlias, FieldAlias, FieldType] = RelationField[RelationAlias, FieldAlias, FieldType]

  given cast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U: DbType](using base: RelationFieldsFactory[RelationAlias, F, G]): RelationFieldsFactory[RelationAlias, Cast[F, U], Cast[G, U]] with
    def value: Cast[G, U] = Cast(base.value)

  given softCast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U: DbType](using base: RelationFieldsFactory[RelationAlias, F, G]): RelationFieldsFactory[RelationAlias, SoftCast[F, U], SoftCast[G, U]] with
    def value: SoftCast[G, U] = SoftCast(base.value)
