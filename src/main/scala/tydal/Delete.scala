package tydal

import tydal.compiler.CommandCompiler

class DeleteCommand[TableName, TableColumns, Where <: LogicalExpr](
  val table: TableSchema[TableName, TableColumns],
  val where: Where
) extends CommandDsl:

  def where[NewWhere <: LogicalExpr](f: Selectable[TableColumns] => NewWhere): DeleteCommand[TableName, TableColumns, NewWhere] =
    DeleteCommand(table, f(table))


object Delete:
  def from[TableName, TableColumns](table: TableSchema[TableName, TableColumns]): DeleteCommand[TableName, TableColumns, AlwaysTrue] =
    DeleteCommand(table, AlwaysTrue)
