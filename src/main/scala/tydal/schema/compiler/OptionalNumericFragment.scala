package tydal.schema.compiler

trait OptionalNumericFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object OptionalNumericFragment:
  given empty: OptionalNumericFragment[None.type, EmptyTuple] with
    override def build(x: None.type) = CompiledQueryFragment.empty

  given nonEmpty[A <: Int]: OptionalNumericFragment[Some[A], EmptyTuple] with
    override def build(x: Some[A]) = CompiledQueryFragment(x.value.toString)
