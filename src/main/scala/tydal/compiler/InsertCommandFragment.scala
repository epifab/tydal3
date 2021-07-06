package tydal.compiler

import tydal._
import Tuple.Concat

trait InsertCommandFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object InsertCommandFragment:
  given insert[TableName, TableColumns, KeyValues, A <: Tuple, B <: Tuple] (
    using
    fields: ListFragment[KeyFragment, KeyValues, A],
    values: ListFragment[ValueFragment, KeyValues, B]
  ): InsertCommandFragment[InsertCommand[TableName, TableColumns, KeyValues], A Concat B] with
    def build(command: InsertCommand[TableName, TableColumns, KeyValues]): CompiledFragment[A Concat B] =
      CompiledFragment(s"INSERT INTO ${command.table.name.value}") ++
        fields.build(command.keyValues, ", ").wrap("(", ")") ++
        values.build(command.keyValues, ", ").wrap("VALUES (", ")")
