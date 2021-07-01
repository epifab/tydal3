package tydal.schema.compiler

import tydal.schema._

trait OptionalPlaceholderFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object OptionalPlaceholderFragment:
  given empty: OptionalPlaceholderFragment[None.type, EmptyTuple] with
    override def build(x: None.type) = CompiledFragment.empty

  given nonEmpty[A, T, Input <: Tuple](using fragment: FieldFragment[Placeholder[A, T], Input]): OptionalPlaceholderFragment[Some[Placeholder[A, T]], Input] with
    override def build(x: Some[Placeholder[A, T]]) = fragment.build(x.value)
