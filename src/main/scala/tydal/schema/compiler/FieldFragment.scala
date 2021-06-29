package tydal.schema.compiler

import tydal.schema._

trait FieldFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]
trait FieldAsAliasFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]
trait FieldAliasFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object FieldFragment:

  given aliased[T, F <: Field[T], A, Output <: Tuple] (
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[Aliased[T, F, A], Output] with
    def build(field: Aliased[T, F, A]): CompiledFragment[Output] = inner.build(field.field)

  given fieldRef[Src, Alias, T]: FieldFragment[RelationField[Src, Alias, T], EmptyTuple] with
    def build(field: RelationField[Src, Alias, T]): CompiledFragment[EmptyTuple] =
      CompiledFragment(s"${field.relationAlias.value}.${field.name.value}")

  given cast[F <: Field[_], Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[Cast[F, _], Output] with
    def build(field: Cast[F, _]): CompiledFragment[Output] = inner.build(field.field).append("::" + field.dbType.dbName)

  given softCast[F <: Field[_], T, Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[SoftCast[F, T], Output] with
    def build(field: SoftCast[F, T]): CompiledFragment[Output] = inner.build(field.field)

  given dbFunction[FS <: Tuple, T, Output <: Tuple](
    using
    inner: CommaSeparatedListFragment[FieldFragment, FS, Output]
  ): FieldFragment[DbFunction[FS, T], Output] with
    def build(func: DbFunction[FS, T]): CompiledFragment[Output] =
      inner.build(func.params).wrap(s"${func.dbName}(", ")")

  given placeholder[P <: Placeholder[_, _]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledFragment[P *: EmptyTuple] = CompiledFragment(List(placeholder.dbType.codec.sql), placeholder *: EmptyTuple)

  given literal[P <: Literal[_]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledFragment[P *: EmptyTuple] = CompiledFragment(List(placeholder.dbType.codec.sql), placeholder *: EmptyTuple)


trait FieldAsAliasSrc:
  given unAliased[A, F <: Field[_], O <: Tuple] (using field: FieldFragment[F, O]): FieldAsAliasFragment[F, O] with
    def build(x: F): CompiledFragment[O] = field.build(x)

object FieldAsAliasFragment extends FieldAsAliasSrc:
  given aliased[T, F <: Field[T], A, O <: Tuple](using field: FieldFragment[F, O]): FieldAsAliasFragment[Aliased[T, F, A], O] with
    def build(x: Aliased[T, F, A]): CompiledFragment[O] = field.build(x.field).append(s" AS ${x.alias.value}")


trait FieldAliasSrc:
  given unAliased[A, F <: Field[_], O <: Tuple] (using field: FieldFragment[F, O]): FieldAliasFragment[F, O] with
    def build(x: F): CompiledFragment[O] = field.build(x)

object FieldAliasFragment extends FieldAliasSrc:
  given aliased[T, F <: Field[T], A]: FieldAsAliasFragment[Aliased[T, F, A], EmptyTuple] with
    def build(x: Aliased[T, F, A]): CompiledFragment[EmptyTuple] = CompiledFragment(x.alias.value)
