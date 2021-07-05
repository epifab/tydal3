package tydal

final class Cast[+F <: Field[_], U](val field: F)(using val dbType: DbType[U]) extends Field[U]:
  override def toString: String = s"$field::${dbType.dbName}"

final class SoftCast[+F <: Field[_], U](val field: F)(using val dbType: DbType[U]) extends Field[U]:
  override def toString: String = field.toString
