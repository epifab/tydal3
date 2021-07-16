package tydal

final class RelationField[RelationAlias, Name, T](using val relationAlias: DbIdentifier[RelationAlias], val name: DbIdentifier[Name], val dbType: DbType[T]) extends Field[T]:
  override def toString: String = s"${relationAlias.value}.${name.value}"
