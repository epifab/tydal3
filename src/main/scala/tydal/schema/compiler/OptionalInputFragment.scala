package tydal.schema.compiler

import tydal.schema._

trait OptionalInputFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object OptionalInputFragment:
  given empty: OptionalInputFragment[None.type, EmptyTuple] with
    override def build(x: None.type) = CompiledFragment.empty

  given placeholder[A, T, Input <: Tuple](using fragment: FieldFragment[Placeholder[A, T], Input]): OptionalInputFragment[Some[Placeholder[A, T]], Input] with
    override def build(x: Some[Placeholder[A, T]]) = fragment.build(x.value)

  given const[T, Input <: Tuple](using fragment: FieldFragment[Const[T], Input]): OptionalInputFragment[Some[Const[T]], Input] with
    override def build(x: Some[Const[T]]) = fragment.build(x.value)
