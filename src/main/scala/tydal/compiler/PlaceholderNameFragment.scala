package tydal.compiler

import tydal._

trait PlaceholderNameFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object PlaceholderNameFragment:
  given placeholder[Name <: String with Singleton, T]: PlaceholderNameFragment[Placeholder[Name, T], EmptyTuple] with
    def build(x: Placeholder[Name, T]): CompiledFragment[EmptyTuple] = CompiledFragment(x.name.value)
