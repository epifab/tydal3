package tydal

final class Cast[+F <: Field[_], U] private[tydal](val field: F)(using val dbType: DbType[U]) extends Field[U]:
  override def toString: String = s"$field::${dbType.dbName}"

class To[T: DbType: IsNotNullable]

final class SoftCast[+F <: Field[_], U] private[tydal](val field: F)(using val dbType: DbType[U]) extends Field[U]:
  override def toString: String = field.toString
