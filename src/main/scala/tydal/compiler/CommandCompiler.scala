package tydal.compiler

import skunk.Command

trait CommandCompiler[-T, I]:
  def build(command: T): Command[I]

object CommandCompiler:
  given [T, I <: Tuple, IX] (
    using
    origin: skunk.util.Origin,
    fragment: CommandFragment[T, I],
    encoder: EncoderFactory[I, IX]
  ): CommandCompiler[T, IX] with
    def build(command: T): Command[IX] =
      val f = fragment.build(command)
      Command(f.sql, origin, encoder(f.input))
