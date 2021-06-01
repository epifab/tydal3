package tydal.schema

class FieldRef[Src, Name, T](using val src: DbIdentifier[Src], val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = s"${src.value}.${name.value}"

class Column[Name, T](using val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = name.value

type :=:[A, B] = Column[A, B]

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

  given columns[RelationAlias: DbIdentifier, ColumnName: DbIdentifier, ColumnType: DbType, RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple] (
    using
    tailBuilder: FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, Column[ColumnName, ColumnType] *: RawFieldsTail, FieldRef[RelationAlias, ColumnName, ColumnType] *: ProcessedFieldsTail] with { }

  given refs[RelationAlias: DbIdentifier, FieldSrc, FieldAlias: DbIdentifier, FieldType: DbType, RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple] (
    using
    FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, FieldRef[FieldSrc, FieldAlias, FieldType] *: RawFieldsTail, FieldRef[RelationAlias, FieldAlias, FieldType] *: ProcessedFieldsTail] with { }

  given tagged[RelationAlias: DbIdentifier, FieldAlias: DbIdentifier, FieldType: DbType, F <: Field[FieldType], RawFieldsTail <: Tuple, ProcessedFieldsTail <: Tuple] (
    using
    FieldRefs[RelationAlias, RawFieldsTail, ProcessedFieldsTail]
  ): FieldRefs[RelationAlias, Tagged[F, FieldAlias] *: RawFieldsTail, FieldRef[RelationAlias, FieldAlias, FieldType] *: ProcessedFieldsTail] with { }
