package tydal.schema

import tydal.schema

final class Placeholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Field[T]

object Placeholder:
  type Aux[Name <: String with Singleton, T, U] = Placeholder[Name, T] { type Out = U }

case class KeyValue[A <: String with Singleton, +T](key: A, value: T)

trait ColumnPlaceholders[-Columns, Placeholders]:
  def value: Placeholders

object ColumnPlaceholders:
  given empty: ColumnPlaceholders[EmptyTuple, EmptyTuple] with
    def value: EmptyTuple = EmptyTuple

  given column[Name <: String with Singleton, T: DbType](using singleton: ValueOf[Name]): ColumnPlaceholders[Column[Name, T], Placeholder[Name, T]] with
    def value: Placeholder[Name, T] = Placeholder[Name, T](singleton.value)

  given head[Head, HeadPlaceholder, Tail <: Tuple, TailPlaceholders <: Tuple](
                                                                               using
                                                                               head: ColumnPlaceholders[Head, HeadPlaceholder],
                                                                               tail: ColumnPlaceholders[Tail, TailPlaceholders]
  ): ColumnPlaceholders[Head *: Tail, HeadPlaceholder *: TailPlaceholders] with
    def value: HeadPlaceholder *: TailPlaceholders = head.value *: tail.value


extension[Name <: String with Singleton](name: Name)
  def placeholder[T: DbType]: Placeholder[Name, T] = Placeholder(name)

extension[Key <: String with Singleton](key: Key)
  def ~~>[T](value: T): KeyValue[Key, T] = KeyValue(key, value)
