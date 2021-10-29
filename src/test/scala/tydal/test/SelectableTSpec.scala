package tydal.test

import tydal.*

object SelectableTSpec:
  summon[SelectableT[SubQuery["hello", RelationField["h", "w", int4] *: EmptyTuple, Nothing], int4]]
  summon[SelectableT[SubQuery["hello", RelationField["h", "w", int4], Nothing], int4]]
  summon[SelectableT[SelectQuery[Nothing, RelationField["h", "w", int4] *: EmptyTuple, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], int4]]
  summon[SelectableT[SelectQuery[Nothing, RelationField["h", "w", int4], Nothing, Nothing, Nothing, Nothing, Nothing, Nothing], int4]]
