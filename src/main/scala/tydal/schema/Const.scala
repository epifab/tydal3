package tydal.schema

final class Const[T](val dbType: DbType[T], val value: dbType.Out) extends Field[T]

extension[U](value: U)
  def apply[T](using dbt: DbType.Aux[T, U]): Const[T] = Const(dbt, value)
