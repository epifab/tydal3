package tydal
package compiler

trait FieldFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]
trait FieldAsAliasFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]
trait FieldAliasFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object FieldFragment:

  given aliased[T, F <: Field[T], A, Output <: Tuple] (
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[Aliased[T, F, A], Output] with
    def build(field: Aliased[T, F, A]): CompiledFragment[Output] = inner.build(field.field)

  given column[Name, T]: FieldFragment[Column[Name, T], EmptyTuple] with
    def build(field: Column[Name, T]): CompiledFragment[EmptyTuple] =
      CompiledFragment(field.name.escaped)

  given relationField[Src, Alias, T]: FieldFragment[RelationField[Src, Alias, T], EmptyTuple] with
    def build(field: RelationField[Src, Alias, T]): CompiledFragment[EmptyTuple] =
      CompiledFragment(s"${field.relationAlias.escaped}.${field.name.escaped}")

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

  given wrapper[T, F <: Field[T], Output <: Tuple](
    using
    inner: FieldFragment[F, Output]
  ): FieldFragment[ExprWrapper[T, F], Output] with
    def build(wrapper: ExprWrapper[T, F]): CompiledFragment[Output] = inner.build(wrapper.expr).wrap("(", ")")

  given dbFunction[FS <: Tuple, T, Output <: Tuple](
    using
    inner: ListFragment[FieldFragment, FS, Output]
  ): FieldFragment[DbFunction[FS, T], Output] with
    def build(func: DbFunction[FS, T]): CompiledFragment[Output] =
      func match
        case f: Aggregation[_, _] if f.distinct => inner.build(func.params, ", ").wrap(s"${func.dbName}(DISTINCT ", ")")
        case f: DbFunction2[_, _, _] if f.infixNotation => inner.build(func.params, s" ${func.dbName} ")
        case _ => inner.build(func.params, ", ").wrap(s"${func.dbName}(", ")")

  given placeholder[P <: Placeholder[_, _]]: FieldFragment[P, P *: EmptyTuple] with
    def build(placeholder: P): CompiledFragment[P *: EmptyTuple] = CompiledFragment(List(placeholder.dbType.codec.sql), placeholder *: EmptyTuple)

  given const[P <: Const[_]]: FieldFragment[P, P *: EmptyTuple] with
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
