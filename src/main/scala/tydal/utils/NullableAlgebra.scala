package tydal
package utils

trait NullableAlgebra[-T, X, U]:
  def get: DbType[U]

/**
 * Given a T and X, it builds a U as nullable[X] if either T is nullable[_]
 *  or T is a tuple made of nullable[_], U = X otherwise
 * @tparam T
 * @tparam X
 * @tparam U
 */
trait EveryNullableAlgebra[-T, X, U] extends NullableAlgebra[T, X, U]

/**
 * Given a T and X, it builds a U as nullable[X] if either T is nullable[_]
 *  or T is a tuple made of at least one nullable[_], U = X otherwise
 * @tparam T
 * @tparam X
 * @tparam U
 */
trait AnyNullableAlgebra[-T, X, U] extends NullableAlgebra[T, X, U]


trait EveryNotNullableAlgebra:
  given notNullableField[T, X](using dbt: DbType[X]): EveryNullableAlgebra[T, X, X] with
    val get: DbType[X] = dbt

object EveryNullableAlgebra extends EveryNotNullableAlgebra:
  given nullableField[T: IsNullable, X](using dbt: DbType[nullable[X]]): EveryNullableAlgebra[T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given empty[X](using dbt: DbType[nullable[X]]): EveryNullableAlgebra[EmptyTuple, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given nonEmpty[H: IsNullable, T <: Tuple, X, U](using n: EveryNullableAlgebra[T, X, U]): EveryNullableAlgebra[H *: T, X, U] with
    val get: DbType[U] = n.get


trait LowPriorityAnyNullableAlgebra:
  given [T, X](using dbt: DbType[X]): AnyNullableAlgebra[T, X, X] with
    val get: DbType[X] = dbt

trait AnyTailNullableAlgebra extends LowPriorityAnyNullableAlgebra:
  given tail[H, T <: Tuple, X](using n: AnyNullableAlgebra[T, X, nullable[X]]): AnyNullableAlgebra[H *: T, X, nullable[X]] with
    val get: DbType[nullable[X]] = n.get

object AnyNullableAlgebra extends AnyTailNullableAlgebra:
  given oneField[T: IsNullable, X](using dbt: DbType[nullable[X]]): AnyNullableAlgebra[T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given head[H: IsNullable, T <: Tuple, X](using dbt: DbType[nullable[X]]): AnyNullableAlgebra[H *: T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt
