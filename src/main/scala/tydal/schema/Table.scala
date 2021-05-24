package tydal.schema

class Table[Name, Columns](
  using
  val name: DbIdentifier[Name],
  val columns: ColumnsBuilder[Columns]
) extends Taggable with Selectable[Columns]:

  def apply[ColumnName <: Singleton, Column](name: ColumnName)(
    using
    finder: Finder[Columns, Column, ColumnName]
  ): Column = finder.find(columns.get)
