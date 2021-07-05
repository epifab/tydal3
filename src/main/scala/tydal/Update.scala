package tydal

import tydal.compiler.CommandCompiler

class UpdateCommand[TableName, TableColumns, KeyValues, Where <: LogicalExpr](
  val table: TableSchema[TableName, TableColumns],
  val keyValues: KeyValues,
  val where: Where
):
  def fields[Fields, NewKeyValues](f: Selectable[TableColumns] => Fields)(
    using
    newPlaceholders: ColumnPlaceholders[Fields, NewKeyValues]
  ): UpdateCommand[TableName, TableColumns, NewKeyValues, Where] =
    UpdateCommand(table, newPlaceholders.value, where)

  def where[NewWhere <: LogicalExpr](f: Selectable[TableColumns] => NewWhere): UpdateCommand[TableName, TableColumns, KeyValues, NewWhere] =
    UpdateCommand(table, keyValues, f(table))

  def compile[Input <: Tuple](using compiler: CommandCompiler[this.type, Input]): skunk.Command[Input] =
    compiler.build(this)


object Update:
  def apply[TableName, TableColumns, Placeholders](table: TableSchema[TableName, TableColumns])(
    using
    placeholders: ColumnPlaceholders[TableColumns, Placeholders]
  ): UpdateCommand[TableName, TableColumns, Placeholders, AlwaysTrue] = UpdateCommand(table, placeholders.value, AlwaysTrue)
