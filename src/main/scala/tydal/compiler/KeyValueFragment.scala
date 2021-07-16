package tydal
package compiler

import scala.Tuple.Concat

trait KeyValueFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object KeyValueFragment:
  given [K <: String with Singleton, V, A <: Tuple, B <: Tuple](
    using
    keyFragment: KeyFragment[K ~~> V, A],
    valueFragment: ValueFragment[K ~~> V, B]
  ): KeyValueFragment[K ~~> V, A Concat B] with
    def build(x: K ~~> V): CompiledFragment[A Concat B] = keyFragment.build(x).concatenateRequired(valueFragment.build(x), " = ")
