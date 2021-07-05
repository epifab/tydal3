package tydal

final class Placeholder[Name, T](using val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]

object Placeholder:
  type Aux[Name <: String with Singleton, T, U] = Placeholder[Name, T] { type Out = U }

case class KeyValue[A <: String with Singleton, +T](key: A, value: T)

type ~~>[A <: String with Singleton, +T] = KeyValue[A, T]

trait ColumnPlaceholders[-Columns, ColumnValues]:
  def value: ColumnValues

object ColumnPlaceholders:
  given column[Name <: String with Singleton, T: DbType](using singleton: ValueOf[Name], dbi: DbIdentifier[Name]): ColumnPlaceholders[Column[Name, T], (Name ~~> Placeholder[Name, T])] with
    def value: Name ~~> Placeholder[Name, T] = singleton.value ~~> Placeholder[Name, T]

  given empty: ColumnPlaceholders[EmptyTuple, EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given head[Head, HeadPlaceholder, Tail <: Tuple, TailPlaceholders <: Tuple](
    using
    head: ColumnPlaceholders[Head, HeadPlaceholder],
    tail: ColumnPlaceholders[Tail, TailPlaceholders]
  ): ColumnPlaceholders[Head *: Tail, HeadPlaceholder *: TailPlaceholders] with
    def value: HeadPlaceholder *: TailPlaceholders = head.value *: tail.value

extension[Key <: String with Singleton](key: Key)
  def ~~>[T](value: T): KeyValue[Key, T] = KeyValue(key, value)
