package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryCompiler[-Query, Input, Output]:
  def build(query: Query): CompiledQuery[Input, Output]

case class CompiledQuery[Input, Output](sql: String, input: Input, output: Output)

object QueryCompiler:
  given select[
    Fields, FieldsOutput <: Tuple,
    From <: Relations, FromOutput <: Tuple,
    Where <: LogicalExpr, WhereOutput <: Tuple,
    GroupBy, GroupByOutput <: Tuple,
    Having <: LogicalExpr, HavingOutput <: Tuple,
    SortBy <: Tuple, SortByOutput <: Tuple,
    Offset <: Option[Int], OffsetOutput <: Tuple,
    Limit <: Option[Int], LimitOutput <: Tuple
  ](
     using
     fields: CommaSeparatedListFragment[FieldAsAliasFragment, Fields, FieldsOutput],
     from: RelationsFragment[From, FromOutput],
     where: LogicalExprFragment[Where, WhereOutput],
     groupBy: CommaSeparatedListFragment[FieldFragment, GroupBy, GroupByOutput],
     having: LogicalExprFragment[Having, HavingOutput],
     sortBy: CommaSeparatedListFragment[SortByFragment, SortBy, SortByOutput],
     offset: OptionalNumericFragment[Offset, OffsetOutput],
     limit: OptionalNumericFragment[Limit, LimitOutput]
  ): QueryCompiler[
    SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit],
    FieldsOutput Concat FromOutput Concat WhereOutput Concat GroupByOutput Concat HavingOutput Concat SortByOutput Concat OffsetOutput Concat LimitOutput,
    Fields
  ] with
    def build(select: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit]): CompiledQuery[FieldsOutput Concat FromOutput Concat WhereOutput Concat GroupByOutput Concat HavingOutput Concat SortByOutput Concat OffsetOutput Concat LimitOutput, Fields] =
      (fields.build(select.fields).orElse(Some("1")).prepend("SELECT ") `+ +`
        from.build(select.from).prepend("FROM ") `+ +`
        where.build(select.where).prepend("WHERE ") `+ +`
        groupBy.build(select.groupBy).prepend("GROUP BY ") `+ +`
        having.build(select.having).prepend("HAVING ") `+ +`
        sortBy.build(select.sortBy).prepend("ORDER BY ") `+ +`
        offset.build(select.offset).prepend("OFFSET ") `+ +`
        limit.build(select.limit).prepend("LIMIT ")
      ).get(select.fields)
