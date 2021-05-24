package tydal.schema


trait Selectable[Fields]:
  def apply[Tag <: Singleton, Needle](tag: Tag)(
    using
    finder: Finder[Fields, Needle, Tag]
  ): Needle


trait SelectContext[Fields, From] extends Selectable[Fields]:

  def fields: Fields
  def from: From

  def apply[Tag <: Singleton, Field](tag: Tag)(
    using
    finder: Finder[Fields, Field, Tag]
  ): Field = finder.find(fields)

  def apply[Tag1 <: Singleton, Tag2 <: Singleton, Source, Field](tag1: Tag1, tag2: Tag2)(
    using
    finder1: Finder[From, Source, Tag1],
    finder2: Finder[Source, Field, Tag2]
  ): Field = finder2.find(finder1.find(from))


final class SelectQuery[Fields, From](
  val fields: Fields,
  val from: From
) extends Taggable with SelectContext[Fields, From]:

  def take[NewFields](f: SelectContext[Fields, From] => NewFields): SelectQuery[NewFields, From] =
    new SelectQuery(f(this), from)

//  def innerJoin[RelationFields, Relation <: Selectable[RelationFields], RelationAlias](relation: Tagged[Relation, RelationAlias], on: (Selectable[RelationFields], Selectable[Fields]) => JoinClause)

object Select:
  def from[Name, Columns, Alias <: Singleton](table: Tagged[Table[Name, Columns], Alias]): SelectQuery[EmptyTuple, Tagged[Table[Name, Columns], Alias] *: EmptyTuple] =
    new SelectQuery[EmptyTuple, Tagged[Table[Name, Columns], Alias] *: EmptyTuple](EmptyTuple, table *: EmptyTuple)
