package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryCompiler[-Query, Input <: Tuple, Output]:
  def build(query: Query): CompiledQuery[Input, Output]

case class CompiledQuery[Input, Output](sql: String, input: Input, output: Output)

object QueryCompiler:
  given select[Input <: Tuple, Output, S <: SelectQuery[_, Output, _, _, _, _, _, _]](
    using
    fragment: SelectQueryFragment[S, Input]
  ): QueryCompiler[S, Input, Output] with
    def build(select: S): CompiledQuery[Input, Output] = ??? // fragment.build(select).get(select.fields)
