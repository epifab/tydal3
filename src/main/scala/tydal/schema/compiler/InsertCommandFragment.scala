package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait InsertCommandFragment[-T, I <: Tuple] extends FragmentCompiler[T, I]

object InsertCommandFragment:
  given insert[TableName, Placeholders, A <: Tuple, B <: Tuple](
    using
    fields: CommaSeparatedListFragment[PlaceholderNameFragment, Placeholders, A],
    values: CommaSeparatedListFragment[FieldFragment, Placeholders, B]
  ): InsertCommandFragment[InsertCommand[TableName, Placeholders], A Concat B] with
    def build(command: InsertCommand[TableName, Placeholders]): CompiledFragment[A Concat B] =
      CompiledFragment(s"INSERT INTO ${command.tableName.value}") ++
        fields.build(command.placeholders).wrap("(", ")") ++
        values.build(command.placeholders).wrap("VALUES (", ")")
