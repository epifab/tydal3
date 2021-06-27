package tydal.schema.compiler

import tydal.schema.EncoderAdapter
import skunk.Command

trait CommandCompiler[-T, I <: Tuple]:
  def build(command: T): Command[I]

object CommandCompiler:
  given insert[T, I <: Tuple, IX <: Tuple] (
    using
    origin: skunk.util.Origin,
    fragment: InsertCommandFragment[T, I],
    encoder: EncoderAdapter[I, IX]
  ): CommandCompiler[T, IX] with
    def build(command: T): Command[IX] =
      val f = fragment.build(command)
      Command(f.sql, origin, encoder(f.input))
