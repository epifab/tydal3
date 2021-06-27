package tydal.schema

object ListOfPlaceholdersSpec:
  summon[ColumnPlaceholders[Column["hello", varchar], Placeholder["hello", varchar]]]
  summon[ColumnPlaceholders[EmptyTuple, EmptyTuple]]
  summon[ColumnPlaceholders[Column["hello", varchar] *: EmptyTuple, Placeholder["hello", varchar] *: EmptyTuple]]
  summon[ColumnPlaceholders[(Column["hello", varchar], Column["world", integer]), (Placeholder["hello", varchar], Placeholder["world", integer])]]
