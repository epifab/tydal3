package tydal

trait TextFunctions

import tydal.utils.EveryNullableAlgebra

final class CharLength[T: IsText, F <: Field[T], U](override val param: F)(using dn: EveryNullableAlgebra[T, int4, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "CHAR_LENGTH"
  override val dbType: DbType[U] = dn.get

final class Upper[T: IsText, F <: Field[T], U](override val param: F)(using dn: EveryNullableAlgebra[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "UPPER"
  override val dbType: DbType[U] = dn.get

final class Lower[T: IsText, F <: Field[T], U](override val param: F)(using dn: EveryNullableAlgebra[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "LOWER"
  override val dbType: DbType[U] = dn.get

final class Concat[Fields <: Tuple, T](override val params: Fields)(using dn: EveryNullableAlgebra[Fields, text, T]) extends TextFunctions with DbFunction[Fields, T]:
  override val dbName: String = "CONCAT"
  override val dbType: DbType[T] = dn.get

object Concat:
  def apply[F1 <: Field[_], F2 <: Field[_], T](f1: F1, f2: F2)(using EveryNullableAlgebra[(F1, F2), text, T]): Concat[(F1, F2), T] =
    new Concat((f1, f2))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], T](f1: F1, f2: F2, f3: F3)(using EveryNullableAlgebra[(F1, F2, F3), text, T]): Concat[(F1, F2, F3), T] =
    new Concat((f1, f2, f3))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], F4 <: Field[_], T](f1: F1, f2: F2, f3: F3, f4: F4)(using EveryNullableAlgebra[(F1, F2, F3, F4), text, T]): Concat[(F1, F2, F3, F4), T] =
    new Concat((f1, f2, f3, f4))

  def apply[F1 <: Field[_], F2 <: Field[_], F3 <: Field[_], F4 <: Field[_], F5 <: Field[_], T](f1: F1, f2: F2, f3: F3, f4: F4, f5: F5)(using EveryNullableAlgebra[(F1, F2, F3, F4, F5), text, T]): Concat[(F1, F2, F3, F4, F5), T] =
    new Concat((f1, f2, f3, f4, f5))
