package tydal.schema

import tydal.schema

final class Placeholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Field[T]

extension[Name <: String with Singleton](name: Name)
  def placeholder[T: DbType]: Placeholder[Name, T] = Placeholder(name)

object Placeholder:
  type Aux[Name <: String with Singleton, T, U] = Placeholder[Name, T] { type Out = U }

case class KeyValue[A <: String with Singleton, +T](key: A, value: T)

extension[Key <: String with Singleton](key: Key)
  def ~~>[T](value: T): KeyValue[Key, T] = KeyValue(key, value)
