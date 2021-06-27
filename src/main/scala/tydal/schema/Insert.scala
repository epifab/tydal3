package tydal.schema

import tydal.schema.compiler.CommandCompiler

class InsertCommand[TableName, Placeholders](val placeholders: Placeholders)(using val tableName: DbIdentifier[TableName]):
  def compile[Input <: Tuple](using compiler: CommandCompiler[this.type, Input]): skunk.Command[Input] =
    compiler.build(this)

object Insert:
  class InsertCommandStep1[TableName, Columns](using val tableName: DbIdentifier[TableName]):
    def fields[Fields, Placeholders](f: Selectable[Columns] => Fields)(
      using
      placeholders: ColumnPlaceholders[Fields, Placeholders]
    ): InsertCommand[TableName, Placeholders] =
      InsertCommand[TableName, Placeholders](placeholders.value)

  def into[TableName, Columns](table: TableSchema[TableName, Columns]): InsertCommandStep1[TableName, Columns] =
    InsertCommandStep1(using table.name)
