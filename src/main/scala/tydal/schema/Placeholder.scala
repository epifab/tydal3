package tydal.schema

sealed trait Placeholder[T] extends Field[T]

final class NamedPlaceholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Placeholder[T]:
  override def toString: String = s"Placeholder($name)"

extension[Name <: String with Singleton](name: Name)
  def `??`[T: DbType]: NamedPlaceholder[Name, T] = NamedPlaceholder(name)
