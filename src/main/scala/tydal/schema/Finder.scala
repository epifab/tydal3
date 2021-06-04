package tydal.schema


trait Finder[-Haystack, +Needle, Tag]:
  def find(haystack: Haystack): Needle

object Finder:
  given head[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Head, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.head)

  given tail[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Tail, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.tail)

  given ref[Src, Name, Type]: Finder[FieldRef[Src, Name, Type], FieldRef[Src, Name, Type], Name] with
    def find(haystack: FieldRef[Src, Name, Type]): FieldRef[Src, Name, Type] = haystack

  given tagged[Needle, Tag]: Finder[Tagged[Needle, Tag], Needle, Tag] with
    def find(haystack: Tagged[Needle, Tag]): Needle = haystack.item

  given softCast[F <: Field[_], U, Needle, Tag](using finder: Finder[F, Needle, Tag]): Finder[SoftCast[F, U], SoftCast[F, U], Tag] with
    def find(haystack: SoftCast[F, U]): SoftCast[F, U] = haystack

  given relationField[RelationAlias, RelationFields, Needle, Tag](using finder: Finder[RelationFields, Needle, Tag]): Finder[Relation[RelationAlias, RelationFields], Needle, Tag] with
    def find(haystack: Relation[RelationAlias, RelationFields]): Needle = finder.find(haystack.fields)

  given relation[Alias, Fields]: Finder[Relation[Alias, Fields], Relation[Alias, Fields], Alias] with
    def find(haystack: Relation[Alias, Fields]): Relation[Alias, Fields] = haystack

  given joinHead[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](using headFinder: Finder[Head, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = headFinder.find(haystack.head)

  given joinTail[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr](using tailFinder: Finder[Tail, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = tailFinder.find(haystack.tail)
