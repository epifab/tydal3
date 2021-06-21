package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryCompiler[-Query, Input]:
  def build(query: Query): CompiledQuery[Input]

case class CompiledQuery[Input](sql: String, encoder: skunk.Encoder[Input])

object QueryCompiler:
  given select[Input <: Tuple, InputEncoder <: Tuple, S] (
    using
    fragment: SelectQueryFragment[S, Input],
    encoder: EncoderAdapter[Input, InputEncoder]
  ): QueryCompiler[S, InputEncoder] with
    def build(select: S): CompiledQuery[InputEncoder] = {
      val value = fragment.build(select)
      CompiledQuery(value.sql, encoder(value.input))
    }
