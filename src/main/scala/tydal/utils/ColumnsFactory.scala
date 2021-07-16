package tydal
package utils

/**
 * Singleton value for a list of columns
 * @tparam Columns
 */
trait ColumnsFactory[Columns]:
  def value: Columns

object ColumnsFactory:
  given empty: ColumnsFactory[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Name, T, Tail <: Tuple](using dbi: DbIdentifier[Name], dbt: DbType[T], tail: ColumnsFactory[Tail]): ColumnsFactory[Column[Name, T] *: Tail] with
    def value: Column[Name, T] *: Tail = new Column[Name, T] *: tail.value
