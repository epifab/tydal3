package tydal

final class Placeholder[Name, T](using val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]

object Placeholder:
  type Aux[Name <: String with Singleton, T, U] = Placeholder[Name, T] { type Out = U }

case class KeyValue[A <: String with Singleton, +T](key: A, value: T)

type ~~>[A <: String with Singleton, +T] = KeyValue[A, T]

extension[Key <: String with Singleton](key: Key)
  def ~~>[T](value: T): KeyValue[Key, T] = KeyValue(key, value)
