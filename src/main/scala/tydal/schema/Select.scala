package tydal.schema


trait Selectable[Fields]:
  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle


trait SelectableT[-F, T]

object SelectableT:
  given[F, T](using FieldT[F, T]): SelectableT[Selectable[F *: EmptyTuple], T] with { }


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
) extends SelectContext[Fields, From]:

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

  def as[Alias <: String with Singleton, SubQueryFields](alias: Alias)(
    using
    DbIdentifier[Alias],
    FieldRefs[Alias, Fields, SubQueryFields],
    ListOfFields[SubQueryFields]
  ): SubQuery[Alias, SubQueryFields, this.type] = SubQuery(this)


final class SubQuery[Alias, Fields, S](subQuery: S)(using val alias: DbIdentifier[Alias], val fields: ListOfFields[Fields]) extends Selectable[Fields]:

  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle = finder.find(fields.value)


object Select:
  def from[Name, Alias, Columns](table: Table[Name, Alias, Columns]): SelectQuery[Table[Name, Alias, Columns] *: EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple] =
    SelectQuery(table *: EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple, EmptyTuple)
