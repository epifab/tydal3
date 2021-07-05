package tydal.compiler

import tydal._

trait KeyFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object KeyFragment:
  given [K <: String with Singleton, V]: KeyFragment[K ~~> V, EmptyTuple] with
    def build(x: K ~~> V): CompiledFragment[EmptyTuple] = CompiledFragment(x.key)
