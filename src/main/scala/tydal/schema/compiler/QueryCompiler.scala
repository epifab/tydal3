package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryCompiler[-Query, Input, Output]:
  def build(query: Query): CompiledQuery[Input, Output]

case class CompiledQuery[Input, Output](sql: String, encoder: skunk.Encoder[Input], decoder: skunk.Decoder[Output]):
  def query(using origin: skunk.util.Origin): skunk.Query[Input, Output] = skunk.Query(sql, origin, encoder, decoder)

object QueryCompiler:
  given select[Input <: Tuple, InputEncoder <: Tuple, Output, OutputDecoder <: Tuple, S <: SelectQuery[_, Output, _, _, _, _, _, _]] (
    using
    fragment: SelectQueryFragment[S, Input],
    encoder: EncoderAdapter[Input, InputEncoder],
    decoder: DecoderAdapter[Output, OutputDecoder],
  ): QueryCompiler[S, InputEncoder, OutputDecoder] with
    def build(select: S): CompiledQuery[InputEncoder, OutputDecoder] = {
      val value = fragment.build(select)
      CompiledQuery(value.sql, encoder(value.input), decoder(select.fields))
    }
