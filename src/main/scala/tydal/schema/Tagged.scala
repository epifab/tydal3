package tydal.schema

case class Tagged[+T, A](item: T)(using val tag: DbIdentifier[A]):
  override def toString: String = s"$item as ${tag.value}"

trait Taggable

extension[T <: Taggable](taggable: T)
  def as[A](tag: A)(using DbIdentifier[tag.type]): Tagged[T, tag.type] = Tagged(taggable)

trait Finder[-Haystack, +Needle, Tag]:
  def find(haystack: Haystack): Needle

object Finder:
  given fieldByTag[Needle, Tag, Tail <: Tuple]: Finder[Tagged[Needle, Tag] *: Tail, Needle, Tag] with
    def find(haystack: Tagged[Needle, Tag] *: Tail): Needle = haystack.head.item

  given fieldByName[Src, Name, Type, Tail <: Tuple]: Finder[FieldRef[Src, Name, Type] *: Tail, FieldRef[Src, Name, Type], Name] with
    def find(haystack: FieldRef[Src, Name, Type] *: Tail): FieldRef[Src, Name, Type] = haystack.head

  given tail[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Tail, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.tail)

  given relationField[RelationAlias, RelationFields, Needle, Tag] (using finder: Finder[RelationFields, Needle, Tag]): Finder[Relation[RelationAlias, RelationFields], Needle, Tag] with
    def find(haystack: Relation[RelationAlias, RelationFields]): Needle = finder.find(haystack.fields.value)

  given relation[Alias, Fields]: Finder[Relation[Alias, Fields], Relation[Alias, Fields], Alias] with
    def find(haystack: Relation[Alias, Fields]): Relation[Alias, Fields] = haystack

  given joinHead[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](using headFinder: Finder[Head, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = headFinder.find(haystack.head)

  given joinTail[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](using tailFinder: Finder[Tail, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = tailFinder.find(haystack.tail)
