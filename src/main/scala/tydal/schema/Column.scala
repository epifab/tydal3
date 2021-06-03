package tydal.schema

final class Column[Name, T](using val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = name.value

type :=:[A, B] = Column[A, B]

trait ListOfColumns[Columns]:
  def value: Columns

object ListOfColumns:
  given empty: ListOfColumns[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Name, T, Tail <: Tuple](using dbi: DbIdentifier[Name], dbt: DbType[T], tail: ListOfColumns[Tail]): ListOfColumns[Column[Name, T] *: Tail] with
    def value: Column[Name, T] *: Tail = new Column[Name, T] *: tail.value
