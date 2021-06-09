package tydal.schema

object SelectableTSpec:
  summon[SelectableT[SubQuery["hello", RelationField["h", "w", integer] *: EmptyTuple, Nothing], integer]]
