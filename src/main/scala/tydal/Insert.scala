package tydal

import tydal.compiler.CommandCompiler

class InsertCommand[TableName, TableColumns, KeyValues](val table: TableSchema[TableName, TableColumns], val keyValues: KeyValues):
  def fields[Fields, NewPlaceholders](f: Selectable[TableColumns] => Fields)(
    using
    placeholders: ColumnPlaceholders[Fields, NewPlaceholders]
  ): InsertCommand[TableName, TableColumns, NewPlaceholders] =
    InsertCommand[TableName, TableColumns, NewPlaceholders](table, placeholders.value)

  def compile[Input <: Tuple](using compiler: CommandCompiler[this.type, Input]): skunk.Command[Input] =
    compiler.build(this)

object Insert:
  def into[TableName, TableColumns, Placeholders](table: TableSchema[TableName, TableColumns])(
    using
    placeholders: ColumnPlaceholders[TableColumns, Placeholders]
  ): InsertCommand[TableName, TableColumns, Placeholders] = InsertCommand(table, placeholders.value)
