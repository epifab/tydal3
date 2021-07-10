package tydal

final class Column[Name, T](using val name: DbIdentifier[Name], override val dbType: DbType[T]) extends Field[T]:
  override def toString: String = name.value

  def :==[G <: Field[_]](right: G)(using AreComparable[this.type, G]): Assignment[this.type, G] = Assignment(this, right)

type :=:[A, B] = Column[A, B]

trait ListOfColumns[Columns]:
  def value: Columns

object ListOfColumns:
  given empty: ListOfColumns[EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Name, T, Tail <: Tuple](using dbi: DbIdentifier[Name], dbt: DbType[T], tail: ListOfColumns[Tail]): ListOfColumns[Column[Name, T] *: Tail] with
    def value: Column[Name, T] *: Tail = new Column[Name, T] *: tail.value
