package tydal
package utils

/**
 * Given a T and X, it builds a U as nullable[X] if either T is nullable[_]
 *  or T is a tuple made of nullable[_], U = X otherwise
 * @tparam T
 * @tparam X
 * @tparam U
 */
trait EveryNullableAlgebra[-T, X, U]:
  def get: DbType[U]


trait LowPriorityEveryNullableAlgebra:
  given [T, X](using dbt: DbType[X]): EveryNullableAlgebra[T, X, X] with
    val get: DbType[X] = dbt

object EveryNullableAlgebra extends LowPriorityEveryNullableAlgebra:
  given oneField[T: IsNullable, X](using dbt: DbType[nullable[X]]): EveryNullableAlgebra[T, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given empty[X](using dbt: DbType[nullable[X]]): EveryNullableAlgebra[EmptyTuple, X, nullable[X]] with
    val get: DbType[nullable[X]] = dbt

  given nonEmpty[H: IsNullable, T <: Tuple, X] (using n: EveryNullableAlgebra[T, X, nullable[X]]): EveryNullableAlgebra[H *: T, X, nullable[X]] with
    val get: DbType[nullable[X]] = n.get
