package tydal.schema.compiler

import tydal.schema._

trait SortByFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object SortByFragment:
  given ascending[F <: Field[_], I <: Tuple](using fragment: FieldAliasFragment[F, I]): SortByFragment[Asc[F], I] with
    def build(asc: Asc[F]): CompiledFragment[I] = fragment.build(asc.field).append(" asc")

  given descending[F <: Field[_], I <: Tuple](using fragment: FieldAliasFragment[F, I]): SortByFragment[Desc[F], I] with
    def build(asc: Desc[F]): CompiledFragment[I] = fragment.build(asc.field).append(" desc")

  given default[F <: Field[_], I <: Tuple](using fragment: FieldAliasFragment[F, I]): SortByFragment[F, I] with
    def build(field: F): CompiledFragment[I] = fragment.build(field)
