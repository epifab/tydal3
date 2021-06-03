package tydal.schema

final class Cast[F <: Field[_], U](field: F)(using uType: DbType[U]) extends Field[U]:
  def dbType: DbType[U] = uType
  override def toString: String = s"$field::${uType.dbName}"

final class SoftCast[F <: Field[_], U](field: F)(using uType: DbType[U]) extends Field[U]:
  def dbType: DbType[U] = uType
  override def toString: String = field.toString
