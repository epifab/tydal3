package tydal

final class Column[Name, T](using val name: DbIdentifier[Name], override val dbType: DbType[T]) extends Field[T]:
  override def toString: String = name.value

  def :==[G <: Field[_]](right: G)(using AreComparable[this.type, G]): Assignment[this.type, G] = Assignment(this, right)

type :=:[A, B] = Column[A, B]
