package tydal.compiler

import Tuple.Concat

trait CommaSeparatedListFragment[BaseCompiler[a, b <: Tuple] <: FragmentCompiler[a, b], -T, I <: Tuple] extends FragmentCompiler[T, I]

object CommaSeparatedListFragment:
  given emptyTuple[BaseCompiler[a, b <: Tuple] <: FragmentCompiler[a, b]]: CommaSeparatedListFragment[BaseCompiler, EmptyTuple, EmptyTuple] with
    override def build(x: EmptyTuple): CompiledFragment[EmptyTuple] = CompiledFragment.empty

  given nonEmptyTuple[BaseCompiler[a, b <: Tuple] <: FragmentCompiler[a, b], HeadInput, HeadOutput <: Tuple, TailInput <: Tuple, TailOutput <: Tuple] (
    using
    headCompiler: BaseCompiler[HeadInput, HeadOutput],
    tailCompier: CommaSeparatedListFragment[BaseCompiler, TailInput, TailOutput]
  ): CommaSeparatedListFragment[BaseCompiler, HeadInput *: TailInput, HeadOutput Concat TailOutput] with
    def build(tuple: HeadInput *: TailInput): CompiledFragment[HeadOutput Concat TailOutput] =
      headCompiler.build(tuple.head) `+,+` tailCompier.build(tuple.tail)

  given single[X, XO <: Tuple, BaseCompiler[a, b <: Tuple] <: FragmentCompiler[a, b]] (
    using
    compiler: BaseCompiler[X, XO]
  ): CommaSeparatedListFragment[BaseCompiler, X, XO] with
    def build(x: X): CompiledFragment[XO] =
      compiler.build(x)
