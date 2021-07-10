package tydal

import tydal.compiler.CommandCompiler

class InsertCommand[TableName, TableColumns, KeyValues](val table: TableSchema[TableName, TableColumns], val keyValues: KeyValues):
  def fields[Fields: NonEmptyListOfFields, NewKeyValues](f: Selectable[TableColumns] => Fields)(
    using
    assignments: Assignments[Fields, NewKeyValues]
  ): InsertCommand[TableName, TableColumns, NewKeyValues] =
    InsertCommand[TableName, TableColumns, NewKeyValues](table, assignments(f(table)))

  def compile[Input <: Tuple](using compiler: CommandCompiler[this.type, Input]): skunk.Command[Input] =
    compiler.build(this)

object Insert:
  def into[TableName, TableColumns, KeyValues](table: TableSchema[TableName, TableColumns])(
    using
    assignments: Assignments[TableColumns, KeyValues]
  ): InsertCommand[TableName, TableColumns, KeyValues] = InsertCommand(table, assignments(table.fields))
