package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryCompiler[Query, Input <: Tuple, Output <: Tuple]:
  def build(query: Query): CompiledQuery[Input, Output]

case class CompiledQuery[Input <: Tuple, Output <: Tuple](sql: String, input: Input, output: Output)

object QueryCompiler:
  given select[
    From <: Relations, FromOutput <: Tuple,
    Fields <: Tuple, FieldsOutput <: Tuple,
    GroupBy <: Tuple, GroupByOutput <: Tuple,
    Where <: LogicalExpr, WhereOutput <: Tuple,
    Having <: LogicalExpr, HavingOutput <: Tuple,
    SortBy <: Tuple, SortByOutput <: Tuple,
    Offset <: Option[Int], OffsetOutput <: Tuple,
    Limit <: Option[Int], LimitOutput <: Tuple
  ](
     using
     from: RelationFragment[From, FromOutput],
     fields: CommaSeparatedListFragment[FieldAsAliasFragment, Fields, FieldsOutput],
     groupBy: CommaSeparatedListFragment[FieldFragment, GroupBy, GroupByOutput],
     where: LogicalExprFragment[Where, WhereOutput],
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
