package tydal.schema

sealed trait Relations

sealed trait Relation[Alias, Fields] extends Relations with Selectable[Fields]:
  def fields: ListOfFields[Fields]
  def alias: DbIdentifier[Alias]


final class Table[Name, Alias, Fields](using name: DbIdentifier[Name], val alias: DbIdentifier[Alias], val fields: ListOfFields[Fields]) extends Relation[Alias, Fields]:

  def apply[ColumnName <: Singleton, Field](name: ColumnName)(
    using
    finder: Finder[Fields, Field, ColumnName]
  ): Field = finder.find(fields.value)

  override def toString: String = s"${name.value} as ${alias.value}"

final class SubQuery[Alias, Fields, S](subQuery: S)(using val alias: DbIdentifier[Alias], val fields: ListOfFields[Fields]) extends Relation[Alias, Fields]:

  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle = finder.find(fields.value)

enum JoinType:
  case inner, left

final class Join[Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](val tail: Tail, val head: Head, val on: On, val joinType: JoinType) extends Relations:
  override def toString: String = s"$tail $joinType join $head on $on"

trait TableSchema[Name, Columns](using val name: DbIdentifier[Name], val columns: ListOfColumns[Columns]):

  def as[Alias, Fields](alias: Alias)(
    using
    aliasId: DbIdentifier[alias.type],
    fieldRefs: FieldRefs[alias.type, Columns, Fields],
    listOfFields: ListOfFields[Fields]
  ): Table[Name, alias.type, Fields] = Table()

  override def toString: String = name.value
