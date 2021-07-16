package tydal
package compiler

import skunk.Query

trait QueryCompiler[-Q, Input, Output]:
  def build(query: Q): Query[Input, Output]

object QueryCompiler:
  given select[Output, S <: QueryDsl[Output], OutputDecoder, Input <: Tuple, InputEncoder] (
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
