package tydal.schema

object SelectableTSpec:
  summon[SelectableT[SubQuery["hello", FieldRef["h", "w", integer] *: EmptyTuple, Nothing], integer]]
