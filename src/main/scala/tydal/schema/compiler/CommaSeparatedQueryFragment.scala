package tydal.schema.compiler

import Tuple.Concat

trait CommaSeparatedListFragment[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b], -T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object CommaSeparatedListFragment:
  given emptyTuple[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b]]: CommaSeparatedListFragment[BaseCompiler, EmptyTuple, EmptyTuple] with
    override def build(x: EmptyTuple): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(None, x)

  given nonEmptyTuple[BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b], HeadInput, HeadOutput <: Tuple, TailInput <: Tuple, TailOutput <: Tuple](
    using
    headCompiler: BaseCompiler[HeadInput, HeadOutput],
    tailCompier: CommaSeparatedListFragment[BaseCompiler, TailInput, TailOutput]
  ): CommaSeparatedListFragment[BaseCompiler, HeadInput *: TailInput, HeadOutput Concat TailOutput] with
    def build(tuple: HeadInput *: TailInput): CompiledQueryFragment[HeadOutput Concat TailOutput] =
      headCompiler.build(tuple.head) `+,+` tailCompier.build(tuple.tail)

  given single[X, XO <: Tuple, BaseCompiler[a, b <: Tuple] <: QueryFragmentCompiler[a, b]] (
    using
    compiler: BaseCompiler[X, XO]
  ): CommaSeparatedListFragment[BaseCompiler, X, XO] with
    def build(x: X): CompiledQueryFragment[XO] =
      compiler.build(x)
