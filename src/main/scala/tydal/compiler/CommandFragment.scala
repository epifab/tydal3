package tydal.compiler

import tydal._
import Tuple.Concat

trait CommandFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object CommandFragment:
  given insert[TableName, TableColumns, KeyValues, ConflictPolicy <: OnConflict, A <: Tuple, B <: Tuple, C <: Tuple] (
    using
    fields: ListFragment[KeyFragment, KeyValues, A],
    values: ListFragment[ValueFragment, KeyValues, B],
    onConflict: OnConflictFragment[ConflictPolicy, C]
  ): CommandFragment[InsertCommand[TableName, TableColumns, KeyValues, ConflictPolicy], A Concat B Concat C] with
    def build(command: InsertCommand[TableName, TableColumns, KeyValues, ConflictPolicy]): CompiledFragment[A Concat B Concat C] =
      CompiledFragment(s"INSERT INTO ${command.table.name.escaped}") ++
        fields.build(command.keyValues, ", ").wrap("(", ")") ++
        values.build(command.keyValues, ", ").wrap("VALUES (", ")") ++
        onConflict.build(command.conflictPolicy)

  given update[TableName, TableColumns, KeyValues, Where <: LogicalExpr, A <: Tuple, B <: Tuple] (
    using
    keyValues: ListFragment[KeyValueFragment, KeyValues, A],
    where: LogicalExprFragment[Where, B]
  ): CommandFragment[UpdateCommand[TableName, TableColumns, KeyValues, Where], A Concat B] with
    def build(command: UpdateCommand[TableName, TableColumns, KeyValues, Where]): CompiledFragment[A Concat B] =
      CompiledFragment(s"UPDATE ${command.table.name.escaped}") ++
        keyValues.build(command.keyValues, ", ").prepend("SET ") ++
        where.build(command.where).prepend("WHERE ")

  given delete[TableName, TableColumns, Where <: LogicalExpr, A <: Tuple] (
    using
    where: LogicalExprFragment[Where, A]
  ): CommandFragment[DeleteCommand[TableName, TableColumns, Where], A] with
    def build(command: DeleteCommand[TableName, TableColumns, Where]): CompiledFragment[A] =
      CompiledFragment(s"DELETE FROM ${command.table.name.escaped}") ++
        where.build(command.where).prepend("WHERE ")
