package tydal.schema

trait Field[T] extends Taggable:
  def dbType: DbType[T]
