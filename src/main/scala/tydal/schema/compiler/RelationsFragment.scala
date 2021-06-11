package tydal.schema.compiler

import Tuple.Concat
import tydal.schema._

trait RelationsFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object RelationsFragment:
  given table[Name, Alias, Fields]: RelationsFragment[Table[Name, Alias, Fields], EmptyTuple] with
    def build(table: Table[Name, Alias, Fields]): CompiledQueryFragment[EmptyTuple] =
      CompiledQueryFragment(s"${table.name.value} as ${table.alias.value}")

  given join[Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr, TailOutput <: Tuple, HeadOutput <: Tuple, OnOutput <: Tuple](
    using
    tailCompiler: RelationsFragment[Tail, TailOutput],
    headCompiler: RelationsFragment[Head, HeadOutput],
    onCompiler: LogicalExprFragment[On, OnOutput]
  ): RelationsFragment[Join[Tail, Head, On], TailOutput Concat HeadOutput Concat OnOutput] with
    def build(join: Join[Tail, Head, On]): CompiledQueryFragment[TailOutput Concat HeadOutput Concat OnOutput] =
      tailCompiler.build(join.tail) ++ (join.joinType match
        case JoinType.inner => "inner join"
        case JoinType.left => "left join"
      ) ++ headCompiler.build(join.head) ++ "on" ++ onCompiler.build(join.on)
