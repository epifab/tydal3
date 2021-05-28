package tydal.schema

object FieldTSpec:
  summon[FieldT[Column["hello", varchar], varchar]]
  summon[FieldT[FieldRef["hello", "world", varchar], varchar]]
