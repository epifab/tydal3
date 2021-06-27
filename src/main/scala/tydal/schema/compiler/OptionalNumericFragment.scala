package tydal.schema.compiler

trait OptionalNumericFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object OptionalNumericFragment:
  given empty: OptionalNumericFragment[None.type, EmptyTuple] with
    override def build(x: None.type) = CompiledFragment.empty

  given nonEmpty[A <: Int]: OptionalNumericFragment[Some[A], EmptyTuple] with
    override def build(x: Some[A]) = CompiledFragment(x.value.toString)
