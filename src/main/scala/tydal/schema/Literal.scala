package tydal.schema

final class Literal[T](val dbType: DbType[T], val value: dbType.Out) extends Field[T]

extension[U](value: U)
  def literal[T](using dbt: DbType.Aux[T, U]): Literal[T] = Literal(dbt, value)
