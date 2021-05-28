package tydal.schema

class Table[Name, Alias, Fields](using name: DbIdentifier[Name], alias: DbIdentifier[Alias], val fields: ListOfFields[Fields]) extends Selectable[Fields]:

  def apply[ColumnName <: Singleton, Field](name: ColumnName)(
    using
    finder: Finder[Fields, Field, ColumnName]
  ): Field = finder.find(fields.value)

  override def toString: String = s"${name.value} as ${alias.value}"


class TableSchema[Name, Columns](using val name: DbIdentifier[Name], val columns: ListOfColumns[Columns]):

  def as[Alias, Fields](alias: Alias)(
    using
    aliasId: DbIdentifier[alias.type],
    fieldRefs: FieldRefs[alias.type, Columns, Fields],
    listOfFields: ListOfFields[Fields]
  ): Table[Name, alias.type, Fields] = Table()

  override def toString: String = name.value
