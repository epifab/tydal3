package tydal.schema

trait Field[+T]:
  def as[Alias <: Singleton](alias: Alias)(using DbIdentifier[Alias]): Tagged[Field[T], Alias]
