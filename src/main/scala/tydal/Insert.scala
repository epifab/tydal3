package tydal

import tydal.compiler.CommandCompiler

sealed trait OnConflict

object OnConflict:
  sealed trait ThrowException extends OnConflict
  object ThrowException extends ThrowException

  case class DoNothing[ConflictFields: NonEmptyListOfFields](fields: ConflictFields) extends OnConflict
  case class DoUpdate[ConflictFields: NonEmptyListOfFields, KeyValues](fields: ConflictFields, keyValues: KeyValues) extends OnConflict


class OnConflictBuilder[TableName, TableColumns, KeyValues, ConflictFields: NonEmptyListOfFields](table: TableSchema[TableName, TableColumns], keyValues: KeyValues, conflictFields: ConflictFields):
  def doNothing: InsertCommand[TableName, TableColumns, KeyValues, OnConflict.DoNothing[ConflictFields]] =
    InsertCommand(table, keyValues, OnConflict.DoNothing(conflictFields))

  def doUpdate[A, UpdateKeyValues](f: Selectable[TableColumns] => A)(
    using
    assignments: Assignments[A, UpdateKeyValues]
  ): InsertCommand[TableName, TableColumns, KeyValues, OnConflict.DoUpdate[ConflictFields, UpdateKeyValues]] =
    InsertCommand(table, keyValues, OnConflict.DoUpdate(conflictFields, assignments(f(table))))


class InsertCommand[TableName, TableColumns, KeyValues, ConflictPolicy <: OnConflict](
  val table: TableSchema[TableName, TableColumns],
  val keyValues: KeyValues,
  val conflictPolicy: ConflictPolicy
) extends CommandDsl:

  def fields[Fields: NonEmptyListOfFields, NewKeyValues](f: Selectable[TableColumns] => Fields)(
    using
    assignments: Assignments[Fields, NewKeyValues]
  ): InsertCommand[TableName, TableColumns, NewKeyValues, ConflictPolicy] =
    InsertCommand[TableName, TableColumns, NewKeyValues, ConflictPolicy](table, assignments(f(table)), conflictPolicy)

  def onConflict[ConflictFields: NonEmptyListOfFields](f: Selectable[TableColumns] => ConflictFields): OnConflictBuilder[TableName, TableColumns, KeyValues, ConflictFields] =
    OnConflictBuilder(table, keyValues, f(table))


object Insert:
  def into[TableName, TableColumns, KeyValues](table: TableSchema[TableName, TableColumns])(
    using
    assignments: Assignments[TableColumns, KeyValues]
  ): InsertCommand[TableName, TableColumns, KeyValues, OnConflict.ThrowException] = InsertCommand(table, assignments(table.fields), OnConflict.ThrowException)
