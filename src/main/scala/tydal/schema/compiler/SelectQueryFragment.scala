package tydal.schema.compiler

import tydal.schema._
import tydal.schema.{LogicalExpr, Relations}
import Tuple.Concat

trait SelectQueryFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object SelectQueryFragment:
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
     fields: CommaSeparatedListFragment[FieldAsAliasFragment, Fields, FieldsInput],
     from: RelationsFragment[From, FromInput],
     where: LogicalExprFragment[Where, WhereInput],
     groupBy: CommaSeparatedListFragment[FieldFragment, GroupBy, GroupByInput],
     having: LogicalExprFragment[Having, HavingInput],
     sortBy: CommaSeparatedListFragment[SortByFragment, SortBy, SortByInput],
     offset: OptionalInputFragment[Offset, OffsetInput],
     limit: OptionalInputFragment[Limit, LimitInput]
   ): SelectQueryFragment[
    SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit],
    FieldsInput Concat FromInput Concat WhereInput Concat GroupByInput Concat HavingInput Concat SortByInput Concat OffsetInput Concat LimitInput
  ] with
    def build(select: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit]): CompiledFragment[FieldsInput Concat FromInput Concat WhereInput Concat GroupByInput Concat HavingInput Concat SortByInput Concat OffsetInput Concat LimitInput] =
      fields.build(select.fields).orElse("1").prepend("SELECT ") ++
       from.build(select.from).prepend("FROM ") ++
       where.build(select.where).prepend("WHERE ") ++
       groupBy.build(select.groupBy).prepend("GROUP BY ") ++
       having.build(select.having).prepend("HAVING ") ++
       sortBy.build(select.sortBy).prepend("ORDER BY ") ++
       offset.build(select.offset).prepend("OFFSET ") ++
       limit.build(select.limit).prepend("LIMIT ")
