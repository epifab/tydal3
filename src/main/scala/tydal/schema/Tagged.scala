package tydal.schema

case class Tagged[+T, A](item: T)(using val tag: DbIdentifier[A]):
  override def toString: String = s"$item as ${tag.value}"

trait Taggable

extension[T <: Taggable](taggable: T)
  def as[A](tag: A)(using DbIdentifier[tag.type]): Tagged[T, tag.type] = Tagged(taggable)

trait Finder[Haystack, Needle, Tag]:
  def find(haystack: Haystack): Needle

object Finder:
  given byTag[Needle, Tag, Tail <: Tuple]: Finder[Tagged[Needle, Tag] *: Tail, Needle, Tag] with
    def find(haystack: Tagged[Needle, Tag] *: Tail): Needle = haystack.head.item

  given byColumnName[Name, Type, Tail <: Tuple]: Finder[Column[Name, Type] *: Tail, Column[Name, Type], Name] with
    def find(haystack: Column[Name, Type] *: Tail): Column[Name, Type] = haystack.head

  given tail[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Tail, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.tail)

  given table[Name, Fields, Needle, Tag](using finder: Finder[Fields, Needle, Tag]): Finder[Table[Name, Fields], Needle, Tag] with
    def find(haystack: Table[Name, Fields]): Needle = finder.find(haystack.columns.get)
