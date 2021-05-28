package tydal.schema


trait Selectable[Fields]:
  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle


trait SelectContext[Fields, From] extends Selectable[Fields]:

  def fields: Fields
  def from: From

  def apply[Tag <: Singleton, Field](tag: Tag)(
    using
    finder: Finder[Fields, Field, Tag]
  ): Field = finder.find(fields)

  def apply[Tag1 <: Singleton, Tag2 <: Singleton, Source, Field](tag1: Tag1, tag2: Tag2)(
    using
    finder1: Finder[From, Source, Tag1],
    finder2: Finder[Source, Field, Tag2]
  ): Field = finder2.find(finder1.find(from))


final class SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit](
  val from: From,
  val fields: Fields,
  val groupBy: GroupBy,
  val where: Where,
  val having: Having,
  val sortBy: SortBy,
  val offset: Offset,
  val limit: Limit
) extends Taggable with SelectContext[Fields, From]:

  def take[NewFields <: Tuple](f: SelectContext[Fields, From] => NewFields): SelectQuery[From, NewFields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, f(this), groupBy, where, having, sortBy, offset, limit)

  def groupBy[NewGroupBy <: Tuple](f: SelectContext[Fields, From] => NewGroupBy): SelectQuery[From, Fields, NewGroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, f(this), where, having, sortBy, offset, limit)

  def where[NewWhere <: LogicalExpr](f: SelectContext[Fields, From] => NewWhere): SelectQuery[From, Fields, GroupBy, NewWhere, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, f(this), having, sortBy, offset, limit)

  def having[NewHaving <: LogicalExpr](f: SelectContext[Fields, From] => NewHaving): SelectQuery[From, Fields, GroupBy, Where, NewHaving, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, f(this), sortBy, offset, limit)

  def sortBy[NewSortBy <: Tuple](f: SelectContext[Fields, From] => NewSortBy): SelectQuery[From, Fields, GroupBy, Where, NewSortBy, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, f(this), sortBy, offset, limit)

object Select:
  def from[Name, Columns, Alias <: Singleton](table: Tagged[Table[Name, Columns], Alias]): SelectQuery[Tagged[Table[Name, Columns], Alias] *: EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple] =
    SelectQuery(table *: EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple)
