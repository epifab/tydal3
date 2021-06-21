package tydal.schema

final class Placeholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Field[T]

extension[Name <: String with Singleton](name: Name)
  def placeholder[T: DbType]: Placeholder[Name, T] = Placeholder(name)
