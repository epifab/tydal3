package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait RelationsFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object RelationsFragment:
  given table[Name, Alias, Fields]: RelationsFragment[Table[Name, Alias, Fields], EmptyTuple] with
    def build(table: Table[Name, Alias, Fields]): CompiledFragment[EmptyTuple] =
      CompiledFragment(s"${table.name.value} ${table.alias.value}")

  given subQuery[Alias, Fields, S, T <: Tuple] (using fragment: SelectQueryFragment[S, T]): RelationsFragment[SubQuery[Alias, Fields, S], T] with
    def build(subQuery: SubQuery[Alias, Fields, S]): CompiledFragment[T] =
      fragment.build(subQuery.select)
        .wrap("(", ")")
        .append(s" AS ${subQuery.alias.value}")

  given join[Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr, TailOutput <: Tuple, HeadOutput <: Tuple, OnOutput <: Tuple](
    using
    tailCompiler: RelationsFragment[Tail, TailOutput],
    headCompiler: RelationsFragment[Head, HeadOutput],
    onCompiler: LogicalExprFragment[On, OnOutput]
  ): RelationsFragment[Join[Tail, Head, On], TailOutput Concat HeadOutput Concat OnOutput] with
    def build(join: Join[Tail, Head, On]): CompiledFragment[TailOutput Concat HeadOutput Concat OnOutput] =
      tailCompiler.build(join.tail) ++ (join.joinType match
        case JoinType.inner => "INNER JOIN"
        case JoinType.left => "LEFT JOIN"
      ) ++ headCompiler.build(join.head) ++ onCompiler.build(join.on).prepend("ON ")
