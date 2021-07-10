package tydal

import tydal.compiler.CommandCompiler

class UpdateCommand[TableName, TableColumns, KeyValues, Where <: LogicalExpr](
  val table: TableSchema[TableName, TableColumns],
  val keyValues: KeyValues,
  val where: Where
) extends CommandDsl:

  def set[A, NewKeyValues](f: Selectable[TableColumns] => A)(
    using
    assignments: Assignments[A, NewKeyValues]
  ): UpdateCommand[TableName, TableColumns, NewKeyValues, Where] =
    UpdateCommand(table, assignments(f(table)), where)

  def where[NewWhere <: LogicalExpr](f: Selectable[TableColumns] => NewWhere): UpdateCommand[TableName, TableColumns, KeyValues, NewWhere] =
    UpdateCommand(table, keyValues, f(table))


object Update:
  def apply[TableName, TableColumns, KeyValues](table: TableSchema[TableName, TableColumns])(
    using
    assignments: Assignments[TableColumns, KeyValues]
  ): UpdateCommand[TableName, TableColumns, KeyValues, AlwaysTrue] = UpdateCommand(table, assignments(table.fields), AlwaysTrue)
