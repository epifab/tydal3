package tydal.schema

sealed trait Placeholder[T] extends Field[T]

final class NamedPlaceholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Placeholder[T]:
  override def toString: String = s"Placeholder($name)"

extension[Name <: String with Singleton](name: Name)
  def `??`[T: DbType]: NamedPlaceholder[Name, T] = NamedPlaceholder(name)

final class Literal[T](using val dbType: DbType[T])(val value: dbType.Out) extends Placeholder[T]

final class LiteralOption[T](using val dbType: DbType[nullable[T]])(val value: Option[Literal[T]]) extends Placeholder[nullable[T]]
