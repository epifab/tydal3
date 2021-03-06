package tydal
package compiler

import tydal.utils.*

import scala.Tuple.Concat

trait SelectQueryFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object SelectQueryFragment:
  given simpleSelect[
    Fields,
    FieldsInput <: Tuple
  ](using fields: ListFragment[FieldAsAliasFragment, Fields, FieldsInput]): SelectQueryFragment[SimpleSelect[Fields], FieldsInput] with
    def build(select: SimpleSelect[Fields]): CompiledFragment[FieldsInput] =
      fields.build(select.fields, ", ").orElse("1").prepend("SELECT ")
  
  given select[
    From <: Relations, FromInput <: Tuple,
    Fields, FieldsInput <: Tuple,
    GroupBy, GroupByInput <: Tuple,
    Where <: LogicalExpr, WhereInput <: Tuple,
    Having <: LogicalExpr, HavingInput <: Tuple,
    SortBy, SortByInput <: Tuple,
    Offset: OptionalInt8, OffsetInput <: Tuple,
    Limit: OptionalInt4, LimitInput <: Tuple
  ](
     using
     fields: ListFragment[FieldAsAliasFragment, Fields, FieldsInput],
     from: RelationsFragment[From, FromInput],
     where: LogicalExprFragment[Where, WhereInput],
     groupBy: ListFragment[FieldFragment, GroupBy, GroupByInput],
     having: LogicalExprFragment[Having, HavingInput],
     sortBy: ListFragment[SortByFragment, SortBy, SortByInput],
     offset: OptionalInputFragment[Offset, OffsetInput],
     limit: OptionalInputFragment[Limit, LimitInput]
   ): SelectQueryFragment[
    SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit],
    FieldsInput Concat FromInput Concat WhereInput Concat GroupByInput Concat HavingInput Concat SortByInput Concat OffsetInput Concat LimitInput
  ] with
    def build(select: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit]): CompiledFragment[FieldsInput Concat FromInput Concat WhereInput Concat GroupByInput Concat HavingInput Concat SortByInput Concat OffsetInput Concat LimitInput] =
      fields.build(select.fields, ", ").orElse("1").prepend(if (select.distinct) "SELECT DISTINCT " else "SELECT ") ++
       from.build(select.from).prepend("FROM ") ++
       where.build(select.where).prepend("WHERE ") ++
       groupBy.build(select.groupBy, ", ").prepend("GROUP BY ") ++
       having.build(select.having).prepend("HAVING ") ++
       sortBy.build(select.sortBy, ", ").prepend("ORDER BY ") ++
       offset.build(select.offset).prepend("OFFSET ") ++
       limit.build(select.limit).prepend("LIMIT ")

  given union[FieldsA, A <: QueryDsl[FieldsA], FieldsB, B <: QueryDsl[FieldsB], AInput <: Tuple, BInput <: Tuple] (
    using
    fragmentA: SelectQueryFragment[A, AInput],
    fragmentB: SelectQueryFragment[B, BInput]
  ): SelectQueryFragment[Union[FieldsA, A, FieldsB, B], AInput Concat BInput] with
    def build(union: Union[FieldsA, A, FieldsB, B]): CompiledFragment[AInput Concat BInput] =
      fragmentA.build(union.a) ++ (if (union.distinct) "UNION" else "UNION ALL") ++ fragmentB.build(union.b)
