package tydal.test

import tydal.*

object AssignementsSpec:
  summon[Assignments[Column["hello", varchar], "hello" ~~> Placeholder["hello", varchar]]]
  summon[Assignments[EmptyTuple, EmptyTuple]]
  summon[Assignments[Column["hello", varchar] *: EmptyTuple, "hello" ~~> Placeholder["hello", varchar] *: EmptyTuple]]
  summon[Assignments[(Column["hello", varchar], Column["world", int4]), ("hello" ~~> Placeholder["hello", varchar], "world" ~~> Placeholder["world", int4])]]
