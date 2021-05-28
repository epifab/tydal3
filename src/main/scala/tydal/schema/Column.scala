package tydal.schema

class FieldRef[Src, Name, T](using val src: DbIdentifier[Src], val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = s"${src.value}.${name.value}"

class Column[Name, T](using val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = name.value


trait ListOfColumns[Columns]:
  def value: Columns

object ListOfColumns:
  given empty: ListOfColumns[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Name, T, Tail <: Tuple](using dbi: DbIdentifier[Name], dbt: DbType[T], tail: ListOfColumns[Tail]): ListOfColumns[Column[Name, T] *: Tail] with
    def value: Column[Name, T] *: Tail = new Column[Name, T] *: tail.value


trait ListOfFields[Fields]:
  def value: Fields

object ListOfFields:
  given empty: ListOfFields[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Src, Name, T, Tail <: Tuple](using src: DbIdentifier[Src], name: DbIdentifier[Name], dbt: DbType[T], tail: ListOfFields[Tail]): ListOfFields[FieldRef[Src, Name, T] *: Tail] with
    def value: FieldRef[Src, Name, T] *: Tail = FieldRef[Src, Name, T] *: tail.value


trait FieldRefs[RelationAlias, RawFields, ProcessedFields]

object FieldRefs:
  given empty[RelationAlias]: FieldRefs[RelationAlias, EmptyTuple, EmptyTuple] with { }

  given columns[RelationAlias, ColumnName, ColumnType, RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple](
    using
    src: DbIdentifier[RelationAlias],
    name: DbIdentifier[ColumnName],
    dbType: DbType[ColumnType],
    tailBuilder: FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, Column[ColumnName, ColumnType] *: RawFieldsTail, FieldRef[RelationAlias, ColumnName, ColumnType] *: ProcessedFieldsTail] with { }

  given refs[RelationAlias, FieldSrc, FieldAlias, FieldType, RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple] (
    using
    src: DbIdentifier[RelationAlias],
    name: DbIdentifier[FieldAlias],
    dbType: DbType[FieldType],
    tailBuilder: FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, FieldRef[FieldSrc, FieldAlias, FieldType] *: RawFieldsTail, FieldRef[RelationAlias, FieldAlias, FieldType] *: ProcessedFieldsTail] with { }

  given tagged[RelationAlias, FieldAlias, F <: Field[_], FieldType, RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple](
    using
    src: DbIdentifier[RelationAlias],
    name: DbIdentifier[FieldAlias],
    fieldT: FieldT[F, FieldType],
    dbType: DbType[FieldType],
    tailBuilder: FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, Tagged[F, FieldAlias] *: RawFieldsTail, FieldRef[RelationAlias, FieldAlias, FieldType] *: ProcessedFieldsTail] with { }
