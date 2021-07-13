package tydal

trait TextFunctions

trait DependentNullable[-T, X, U]:
  def get: DbType[U]

trait LowPriorityDependentNullable:
  given [T, X](using dbt: DbType[X]): DependentNullable[T, X, X] with
    val get: DbType[X] = dbt


object DependentNullable extends LowPriorityDependentNullable:
  given oneField[T: IsNullable, X](using dbt: DbType[nullable[X]]): DependentNullable[T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given empty[X](using dbt: DbType[nullable[X]]): DependentNullable[EmptyTuple, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given nonEmpty[H: IsNullable, T <: Tuple, X] (using dn: DependentNullable[T, X, nullable[X]]): DependentNullable[H *: T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dn.get


final class CharLength[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, int4, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "CHAR_LENGTH"
  override val dbType: DbType[U] = dn.get

final class Upper[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "UPPER"
  override val dbType: DbType[U] = dn.get

final class Lower[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "LOWER"
  override val dbType: DbType[U] = dn.get

final class Concat[Fields <: Tuple, T](override val params: Fields)(using dn: DependentNullable[Fields, text, T]) extends TextFunctions with DbFunction[Fields, T]:
  override val dbName: String = "CONCAT"
  override val dbType: DbType[T] = dn.get

object Concat:
  def apply[F1 <: Field[_], F2 <: Field[_], T](f1: F1, f2: F2)(using DependentNullable[(F1, F2), text, T]): Concat[(F1, F2), T] =
    new Concat((f1, f2))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], T](f1: F1, f2: F2, f3: F3)(using DependentNullable[(F1, F2, F3), text, T]): Concat[(F1, F2, F3), T] =
    new Concat((f1, f2, f3))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], F4 <: Field[_], T](f1: F1, f2: F2, f3: F3, f4: F4)(using DependentNullable[(F1, F2, F3, F4), text, T]): Concat[(F1, F2, F3, F4), T] =
    new Concat((f1, f2, f3, f4))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], F4 <: Field[_], F5 <: Field[_], T](f1: F1, f2: F2, f3: F3, f4: F4, f5: F5)(using DependentNullable[(F1, F2, F3, F4, F5), text, T]): Concat[(F1, F2, F3, F4, F5), T] =
    new Concat((f1, f2, f3, f4, f5))
