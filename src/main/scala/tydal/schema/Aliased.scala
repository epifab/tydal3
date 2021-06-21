package tydal.schema

case class Aliased[T, +F <: Field[T], Alias](field: F)(using val alias: DbIdentifier[Alias]) extends Field[T]:
  override val dbType: DbType[T] = field.dbType
  override def toString: String = s"$field as ${alias.value}"
