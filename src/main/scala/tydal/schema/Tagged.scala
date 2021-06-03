package tydal.schema

case class Tagged[+T, A](item: T)(using val tag: DbIdentifier[A]):
  override def toString: String = s"$item as ${tag.value}"

trait Taggable

extension[T <: Taggable](taggable: T)
  def as[A](tag: A)(using DbIdentifier[tag.type]): Tagged[T, tag.type] = Tagged(taggable)
