package tydal.schema

final class FieldRef[Src, Name, T](using val src: DbIdentifier[Src], val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = s"${src.value}.${name.value}"

trait FieldRefs[RelationAlias, RawFields, ProcessedFields]

object FieldRefs:
  given emptyTuple[RelationAlias: DbIdentifier]: FieldRefs[RelationAlias, EmptyTuple, EmptyTuple] with { }

  given nonEmptyTuple[RelationAlias: DbIdentifier, HeadInput, HeadOutput, TailInput <: Tuple, TailOutput <: Tuple] (
    using
    FieldRefs[RelationAlias, HeadInput, HeadOutput],
    FieldRefs[RelationAlias, TailInput, TailOutput]
  ): FieldRefs[RelationAlias, HeadInput *: TailInput, HeadOutput *: TailOutput] with { }

  given column[RelationAlias: DbIdentifier, ColumnName: DbIdentifier, ColumnType: DbType]: FieldRefs[RelationAlias, Column[ColumnName, ColumnType], FieldRef[RelationAlias, ColumnName, ColumnType]] with { }
  given ref[RelationAlias: DbIdentifier, FieldSrc, FieldAlias: DbIdentifier, FieldType: DbType]: FieldRefs[RelationAlias, FieldRef[FieldSrc, FieldAlias, FieldType], FieldRef[RelationAlias, FieldAlias, FieldType]] with { }
  given tagged[RelationAlias: DbIdentifier, FieldAlias: DbIdentifier, FieldType: DbType, F <: Field[FieldType]]: FieldRefs[RelationAlias, Tagged[F, FieldAlias], FieldRef[RelationAlias, FieldAlias, FieldType]] with { }
  given cast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U](using FieldRefs[RelationAlias, F, G]): FieldRefs[RelationAlias, Cast[F, U], Cast[G, U]] with { }
  given softCast[RelationAlias: DbIdentifier, F <: Field[_], G <: Field[_], U](using FieldRefs[RelationAlias, F, G]): FieldRefs[RelationAlias, SoftCast[F, U], SoftCast[G, U]] with { }


trait ListOfFields[Fields]:
  def value: Fields

object ListOfFields:
  given emptyTuple: ListOfFields[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given nonEmptyTuple[Src: DbIdentifier, Name: DbIdentifier, T: DbType, Tail <: Tuple] (using tail: ListOfFields[Tail]): ListOfFields[FieldRef[Src, Name, T] *: Tail] with
    def value: FieldRef[Src, Name, T] *: Tail = FieldRef[Src, Name, T] *: tail.value
