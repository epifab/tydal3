package tydal
package test

extension [T](value: T)
  def is[X](using T =:= X): Unit = ()

object CastSpec:
  Column["foo", int4].cast(To[varchar])
    .is[Cast[Column["foo", int4], varchar]]

  Column["foo", nullable[int4]].cast(To[varchar])
    .is[Cast[Column["foo", nullable[int4]], nullable[varchar]]]
