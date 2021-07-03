package tydal.schema.compiler

import skunk.Query
import tydal.schema._
import Tuple.Concat

trait QueryCompiler[-Q, Input, Output]:
  def build(query: Q): Query[Input, Output]

object QueryCompiler:
  given select[Input <: Tuple, InputEncoder <: Tuple, Output, OutputDecoder, S <: SelectQuery[_, Output, _, _, _, _, _, _]] (
    using
    origin: skunk.util.Origin,
    fragment: SelectQueryFragment[S, Input],
    encoder: EncoderFactory[Input, InputEncoder],
    decoder: DecoderFactory[Output, OutputDecoder],
  ): QueryCompiler[S, InputEncoder, OutputDecoder] with
    def build(select: S): Query[InputEncoder, OutputDecoder] = {
      val value = fragment.build(select)
      Query(value.sql, origin, encoder(value.input), decoder(select.fields))
    }
