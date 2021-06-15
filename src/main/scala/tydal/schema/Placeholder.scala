package tydal.schema

sealed trait Placeholder[T] extends Field[T]

final class NamedPlaceholder[Name <: String with Singleton, T](val name: Name)(using val dbType: DbType[T]) extends Placeholder[T]:
  override def toString: String = s"Placeholder($name)"

final class Literal[T](val dbType: DbType[T], val value: dbType.Out) extends Placeholder[T]
final class LiteralOption[T](val value: Option[Literal[T]])(using val dbType: DbType[nullable[T]]) extends Placeholder[nullable[T]]

extension[Name <: String with Singleton](name: Name)
  def placeholder[T: DbType]: NamedPlaceholder[Name, T] = NamedPlaceholder(name)

extension[U](value: U)
  def literal[T](using dbt: DbType.Aux[T, U]): Literal[T] = Literal(dbt, value)

extension[U](value: Option[U])
  def literalOption[T](using dbt: DbType.Aux[T, U])(using DbType[nullable[T]]): LiteralOption[T] =
    LiteralOption(value.map(Literal(dbt, _)))
