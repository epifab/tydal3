package tydal.schema


trait Selectable[Fields]:
  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle


trait SelectableT[-S, T]

object SelectableT:
  given tuple[S <: Selectable[_], T, F <: Field[T]]: SelectableT[Selectable[F *: EmptyTuple], T] with { }
  given field[S <: Selectable[_], T, F <: Field[T]]: SelectableT[Selectable[F], T] with { }


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


final class SelectQuery[From <: Relations, Fields: ListOfFields, GroupBy: ListOfFields, Where <: LogicalExpr, Having <: LogicalExpr, SortBy <: Tuple, Offset <: Option[Int], Limit <: Option[Int]](
  val from: From,
  val fields: Fields,
  val groupBy: GroupBy,
  val where: Where,
  val having: Having,
  val sortBy: SortBy,
  val offset: Offset,
  val limit: Limit
) extends SelectContext[Fields, From]:

  def take[NewFields: ListOfFields](f: SelectContext[Fields, From] => NewFields): SelectQuery[From, NewFields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, f(this), groupBy, where, having, sortBy, offset, limit)

  def groupBy[NewGroupBy: ListOfFields](f: SelectContext[Fields, From] => NewGroupBy): SelectQuery[From, Fields, NewGroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, f(this), where, having, sortBy, offset, limit)

  def where[NewWhere <: LogicalExpr](f: SelectContext[Fields, From] => NewWhere): SelectQuery[From, Fields, GroupBy, NewWhere, Having, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, f(this), having, sortBy, offset, limit)

  def having[NewHaving <: LogicalExpr](f: SelectContext[Fields, From] => NewHaving): SelectQuery[From, Fields, GroupBy, Where, NewHaving, SortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, f(this), sortBy, offset, limit)

  def sortBy[NewSortBy <: Tuple](f: SelectContext[Fields, From] => NewSortBy): SelectQuery[From, Fields, GroupBy, Where, Having, NewSortBy, Offset, Limit] =
    SelectQuery(from, fields, groupBy, where, having, f(this), offset, limit)

  def innerJoin[RightAlias, RightFields, Right <: Relation[RightAlias, RightFields]](right: Right): JoinBuilder[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit, RightAlias, RightFields, Right] =
    JoinBuilder(this, right, JoinType.inner)

  def leftJoin[RightAlias, RightFields, Right <: Relation[RightAlias, RightFields], NullableFields, NullableRight <: Relation[RightAlias, NullableFields]](right: Right)(using nullable: LooseRelation[RightAlias, RightFields, Right, NullableFields, NullableRight]): JoinBuilder[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit, RightAlias, NullableFields, NullableRight] =
    JoinBuilder(this, nullable(right), JoinType.inner)

  def as[Alias, SubQueryFields](alias: Alias)(
    using
    dbi: DbIdentifier[alias.type],
    fields: RelationFields[alias.type, Fields, SubQueryFields]
  ): SubQuery[alias.type, SubQueryFields, this.type] = SubQuery(fields.value, this)


object Select:
  def from[R <: Relation[_, _]](relation: R): SelectQuery[R, EmptyTuple, EmptyTuple, AlwaysTrue, AlwaysTrue, EmptyTuple, None.type, None.type] =
    SelectQuery(relation, EmptyTuple, EmptyTuple, AlwaysTrue, AlwaysTrue, EmptyTuple, None, None)


class JoinBuilder[
  From <: Relations,
  Fields: ListOfFields,
  GroupBy: ListOfFields,
  Where <: LogicalExpr,
  Having <: LogicalExpr,
  SortBy <: Tuple,
  Offset <: Option[Int],
  Limit <: Option[Int],
  RightAlias,
  RightFields,
  Right <: Relation[RightAlias, RightFields]
](left: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit], right: Right, joinType: JoinType):
  def on[On <: LogicalExpr](f: (Selectable[RightFields], SelectContext[Fields, From]) => On):  SelectQuery[Join[From, Right, On], Fields, GroupBy, Where, Having, SortBy, Offset, Limit] =
    SelectQuery(Join(left.from, right, f(right, left), joinType), left.fields, left.groupBy, left.where, left.having, left.sortBy, left.offset, left.limit)
