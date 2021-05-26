package tydal.schema

class Cast[F <: Field[_], U](field: F)(using uType: DbType[U]) extends Field[U]:
  def dbType: DbType[U] = uType

class SoftCast[F <: Field[_], U](field: F)(using uType: DbType[U]) extends Field[U]:
  def dbType: DbType[U] = uType
