package tydal.schema.compiler

import tydal.schema._

trait FieldFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]
trait FieldAsAliasFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object FieldFragment:

  given tagged[F <: Field[_], A, Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[Tagged[F, A], Output] with
    def build(field: Tagged[F, A]): CompiledQueryFragment[Output] = inner.build(field.item)

  given fieldRef[Src, Alias, T]: FieldFragment[FieldRef[Src, Alias, T], EmptyTuple] with
    def build(field: FieldRef[Src, Alias, T]): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(s"${field.src.value}.${field.name.value}")

  given cast[F <: Field[_], Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[Cast[F, _], Output] with
    def build(field: Cast[F, _]): CompiledQueryFragment[Output] = inner.build(field.field).append("::" + field.dbType.dbName)

  given softCast[F <: Field[_], T, Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[SoftCast[F, T], Output] with
    def build(field: SoftCast[F, T]): CompiledQueryFragment[Output] = inner.build(field.field)

  given dbFunction[FS <: Tuple, T, Output <: Tuple](
    using
    inner: CommaSeparatedListFragment[FieldFragment, FS, Output]
  ): FieldFragment[DbFunction[FS, T], Output] with
    def build(func: DbFunction[FS, T]): CompiledQueryFragment[Output] =
      inner.build(func.params).wrap(s"${func.dbName}(", ")")

  given namedPlaceholder[P <: NamedPlaceholder[_, _]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given literal[P <: Literal[_]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(s"?::${placeholder.dbType.dbName}", placeholder)

  given literalOption[P <: LiteralOption[_]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledQueryFragment[P *: EmptyTuple] = CompiledQueryFragment(Option.when(placeholder.value.isDefined)(s"?::${placeholder.dbType.dbName}"), placeholder *: EmptyTuple)

object FieldAsAliasFragment:

  given tagged[A, F <: Field[_], O <: Tuple](using field: FieldFragment[F, O]): FieldAsAliasFragment[Tagged[F, A], O] with
    def build(x: Tagged[F, A]): CompiledQueryFragment[O] = field.build(x.item).append(s".${x.tag.value}")

  given untagged[A, F <: Field[_], O <: Tuple](using field: FieldFragment[F, O]): FieldAsAliasFragment[F, O] with
    def build(x: F): CompiledQueryFragment[O] = field.build(x)
