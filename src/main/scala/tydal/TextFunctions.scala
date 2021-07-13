package tydal

trait TextFunctions

trait DependentNullable[-T, X, U]:
  def get: DbType[U]

object DependentNullable:
  given [T: IsNullable, X](using dbt: DbType[nullable[X]]): DependentNullable[T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given [T: IsNotNullable, X](using dbt: DbType[X]): DependentNullable[T, X, X] with
    val get: DbType[X] = dbt


final class CharLength[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, int4, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "CHAR_LENGTH"
  override val dbType: DbType[U] = dn.get

final class Upper[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "UPPER"
  override val dbType: DbType[U] = dn.get

final class Lower[T: IsText, F <: Field[T], U](override val param: F)(using dn: DependentNullable[T, text, U]) extends TextFunctions with DbFunction1[F, U]:
  override val dbName: String = "LOWER"
  override val dbType: DbType[U] = dn.get
