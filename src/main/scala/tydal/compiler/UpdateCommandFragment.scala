package tydal.compiler

import tydal._

import scala.Tuple.Concat

trait UpdateCommandFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object UpdateCommandFragment:
  given update[TableName, TableColumns, KeyValues, Where <: LogicalExpr, A <: Tuple, B <: Tuple] (
    using
    keyValues: CommaSeparatedListFragment[KeyValueFragment, KeyValues, A],
    where: LogicalExprFragment[Where, B]
  ): UpdateCommandFragment[UpdateCommand[TableName, TableColumns, KeyValues, Where], A Concat B] with
    def build(command: UpdateCommand[TableName, TableColumns, KeyValues, Where]): CompiledFragment[A Concat B] =
      CompiledFragment(s"UPDATE ${command.table.name.value}") ++
        keyValues.build(command.keyValues).prepend("SET ") ++
        where.build(command.where).prepend("WHERE ")
