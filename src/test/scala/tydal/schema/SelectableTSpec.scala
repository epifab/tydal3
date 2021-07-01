package tydal.schema

object SelectableTSpec:
  summon[SelectableT[SubQuery["hello", RelationField["h", "w", int4] *: EmptyTuple, Nothing], int4]]
