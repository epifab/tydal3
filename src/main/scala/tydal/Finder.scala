package tydal


trait Finder[-Haystack, +Needle, Tag]:
  def find(haystack: Haystack): Needle

trait TailFinder:
  given tail[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Tail, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.tail)

object Finder extends TailFinder:
  given head[Needle, Tag, Head, Tail <: Tuple](using finder: Finder[Head, Needle, Tag]): Finder[Head *: Tail, Needle, Tag] with
    def find(haystack: Head *: Tail): Needle = finder.find(haystack.head)

  given column[Name, Type]: Finder[Column[Name, Type], Column[Name, Type], Name] with
    def find(haystack: Column[Name, Type]): Column[Name, Type] = haystack

  given ref[Src, Name, Type]: Finder[RelationField[Src, Name, Type], RelationField[Src, Name, Type], Name] with
    def find(haystack: RelationField[Src, Name, Type]): RelationField[Src, Name, Type] = haystack

  given aliased[FieldType, Needle <: Field[FieldType], Alias]: Finder[Aliased[FieldType, Needle, Alias], Needle, Alias] with
    def find(haystack: Aliased[FieldType, Needle, Alias]): Needle = haystack.field

  given softCast[F <: Field[_], U, Needle, Tag](using finder: Finder[F, Needle, Tag]): Finder[SoftCast[F, U], SoftCast[F, U], Tag] with
    def find(haystack: SoftCast[F, U]): SoftCast[F, U] = haystack

  given relationField[RelationAlias, RelationFields, Needle, Tag](using finder: Finder[RelationFields, Needle, Tag]): Finder[Relation[RelationAlias, RelationFields], Needle, Tag] with
    def find(haystack: Relation[RelationAlias, RelationFields]): Needle = finder.find(haystack.fields)

  given relation[Alias, Fields, R <: Relation[Alias, Fields]]: Finder[R, R, Alias] with
    def find(haystack: R): R = haystack

  given joinHead[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr] (using headFinder: Finder[Head, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = headFinder.find(haystack.head)

  given joinTail[Alias, Needle, Tail <: Relations, Head <: Relation[_, _], On <: LogicalExpr] (using tailFinder: Finder[Tail, Needle, Alias]): Finder[Join[Tail, Head, On], Needle, Alias] with
    def find(haystack: Join[Tail, Head, On]): Needle = tailFinder.find(haystack.tail)
