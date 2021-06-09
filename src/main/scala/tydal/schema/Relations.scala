package tydal.schema

sealed trait Relations

sealed trait Relation[Alias, Fields] extends Relations with Selectable[Fields]:
  def fields: Fields
  def alias: DbIdentifier[Alias]

  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle = finder.find(fields)


final class Table[Name, Alias, Fields](val fields: Fields)(using val name: DbIdentifier[Name], val alias: DbIdentifier[Alias]) extends Relation[Alias, Fields]:
  override def toString: String = s"${name.value} as ${alias.value}"

final class SubQuery[Alias, Fields, S](val fields: Fields, val subQuery: S)(using val alias: DbIdentifier[Alias]) extends Relation[Alias, Fields]:
  override def toString: String = s"($subQuery) as $alias"


enum JoinType:
  case inner, left

final class Join[Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](val tail: Tail, val head: Head, val on: On, val joinType: JoinType) extends Relations:
  override def toString: String = s"$tail $joinType join $head on $on"

trait TableSchema[Name, Columns](using val name: DbIdentifier[Name], val columns: ListOfColumns[Columns]):

  def as[Alias, Fields](alias: Alias)(
    using
    dbi: DbIdentifier[alias.type],
    fields: RelationFields[alias.type, Columns, Fields]
  ): Table[Name, alias.type, Fields] = Table(fields.value)

  override def toString: String = name.value
