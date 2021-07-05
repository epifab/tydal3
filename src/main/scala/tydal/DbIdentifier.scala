package tydal

trait DbIdentifier[T]:
  def value: String

object DbIdentifier:
  given[A <: String](using singleton: ValueOf[A]): DbIdentifier[A] with
    def value: String = singleton.value
