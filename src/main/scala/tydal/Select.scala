package tydal

import skunk.Query
import tydal.compiler.QueryCompiler
import tydal.utils._


trait Selectable[Fields]:
  def fields: Fields

  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle = finder.find(fields)


trait SelectableT[-S, T]

object SelectableT:
  given tuple[T, F <: Field[T], S <: Selectable[F *: EmptyTuple]]: SelectableT[S, T] with { }
  given field[T, F <: Field[T], S <: Selectable[F]]: SelectableT[S, T] with { }
  given selectContextTupled[T, F <: Field[T], S <: SelectContext[F *: EmptyTuple, _]]: SelectableT[S, T] with { }
  given selectContextField[T, F <: Field[T], S <: SelectContext[F, _]]: SelectableT[S, T] with { }


trait SelectContext[Fields, From]:

  val fields: Fields
  val from: From

  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[(Fields, From), Needle, Tag]
  ): Needle = finder.find((fields, from))

  def apply[Tag1 <: Singleton, Tag2 <: Singleton, Source, Field](tag1: Tag1, tag2: Tag2)(
    using
    finder1: Finder[From, Source, Tag1],
    finder2: Finder[Source, Field, Tag2]
  ): Field = finder2.find(finder1.find(from))


final class SelectQuery[From <: Relations, Fields: NonEmptyListOfFields, GroupBy: ListOfFields, Where <: LogicalExpr, Having <: LogicalExpr, SortBy: SortByClasue, Offset: OptionalInt8, Limit: OptionalInt4](
  val from: From,
  val fields: Fields,
  val groupBy: GroupBy,
  val where: Where,
  val having: Having,
  val sortBy: SortBy,
  val offset: Offset,
  val limit: Limit,
  val distinct: Boolean
) extends QueryDsl[Fields] with SelectContext[Fields, From]:

  def take[NewFields: NonEmptyListOfFields](f: SelectContext[Fields, From] => NewFields): SelectQuery[From, NewFields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, f(this), groupBy, where, having, sortBy, offset, limit, false)

  def takeDistinct[NewFields: NonEmptyListOfFields](f: SelectContext[Fields, From] => NewFields): SelectQuery[From, NewFields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, f(this), groupBy, where, having, sortBy, offset, limit, true)

  def groupBy[NewGroupBy: ListOfFields](f: SelectContext[Fields, From] => NewGroupBy): SelectQuery[From, Fields, NewGroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, f(this), where, having, sortBy, offset, limit, distinct)

  def where[NewWhere <: LogicalExpr](f: SelectContext[Fields, From] => NewWhere): SelectQuery[From, Fields, GroupBy, NewWhere, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, f(this), having, sortBy, offset, limit, distinct)

  def having[NewHaving <: LogicalExpr](f: SelectContext[Fields, From] => NewHaving): SelectQuery[From, Fields, GroupBy, Where, NewHaving, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, f(this), sortBy, offset, limit, distinct)

  def sortBy[NewSortBy: SortByClasue](f: SelectContext[Fields, From] => NewSortBy): SelectQuery[From, Fields, GroupBy, Where, Having, NewSortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, having, f(this), offset, limit, distinct)

  def innerJoin[RightAlias, RightFields, Right <: Relation[RightAlias, RightFields]](right: Right): JoinBuilder[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit, RightAlias, RightFields, Right] =
    JoinBuilder(this, right, JoinType.inner)

  def leftJoin[RightAlias, RightFields, Right <: Relation[RightAlias, RightFields], NullableFields, NullableRight <: Relation[RightAlias, NullableFields]](right: Right)(using nullable: LooseRelation[RightAlias, RightFields, Right, NullableFields, NullableRight]): JoinBuilder[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit, RightAlias, NullableFields, NullableRight] =
    JoinBuilder(this, nullable(right), JoinType.left)

  def limit[PName <: String](limit: PName)(using ValueOf[limit.type]): SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Some[Placeholder[limit.type, int4]]] =
    SelectQuery(from, fields, groupBy, where, having, sortBy, offset, Some(Placeholder[limit.type, int4]), distinct)

  def limit[L <: Const[int4]](limit: L): SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Some[L]] =
    SelectQuery(from, fields, groupBy, where, having, sortBy, offset, Some(limit), distinct)

  def offset[PName <: String](offset: PName)(using ValueOf[offset.type]): SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Some[Placeholder[offset.type, int8]], Limit] =
    SelectQuery(from, fields, groupBy, where, having, sortBy, Some(Placeholder[offset.type, int8]), limit, distinct)

  def offset[L <: Const[int8]](offset: L): SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Some[L], Limit] =
    SelectQuery(from, fields, groupBy, where, having, sortBy, Some(offset), limit, distinct)


final class SimpleSelect[Fields](val fields: Fields) extends QueryDsl[Fields] with Selectable[Fields]


object Select:
  def apply[Fields: NonEmptyListOfFields](fields: Fields): SimpleSelect[Fields] =
    SimpleSelect(fields)

  def from[R <: Relation[_, _]](relation: R): SelectQuery[R, Const[int4], EmptyTuple, AlwaysTrue, AlwaysTrue, EmptyTuple, None.type, None.type] =
    SelectQuery(relation, 1[int4], EmptyTuple, AlwaysTrue, AlwaysTrue, EmptyTuple, None, None, false)


class JoinBuilder[
  From <: Relations,
  Fields: NonEmptyListOfFields,
  GroupBy: ListOfFields,
  Where <: LogicalExpr,
  Having <: LogicalExpr,
  SortBy: SortByClasue,
  Offset: OptionalInt8,
  Limit: OptionalInt4,
  RightAlias,
  RightFields,
  Right <: Relation[RightAlias, RightFields]
](left: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit], right: Right, joinType: JoinType):
  def on[On <: LogicalExpr](f: (Selectable[RightFields], SelectContext[Fields, From]) => On):  SelectQuery[Join[From, Right, On], Fields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(Join(left.from, right, f(right, left), joinType), left.fields, left.groupBy, left.where, left.having, left.sortBy, left.offset, left.limit, left.distinct)
