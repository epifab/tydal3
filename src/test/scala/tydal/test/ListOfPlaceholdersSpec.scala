package tydal.test

import tydal._

object ListOfPlaceholdersSpec:
  summon[ColumnPlaceholders[Column["hello", varchar], "hello" ~~> Placeholder["hello", varchar]]]
  summon[ColumnPlaceholders[EmptyTuple, EmptyTuple]]
  summon[ColumnPlaceholders[Column["hello", varchar] *: EmptyTuple, "hello" ~~> Placeholder["hello", varchar] *: EmptyTuple]]
  summon[ColumnPlaceholders[(Column["hello", varchar], Column["world", int4]), ("hello" ~~> Placeholder["hello", varchar], "world" ~~> Placeholder["world", int4])]]
