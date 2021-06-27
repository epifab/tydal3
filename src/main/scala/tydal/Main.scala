package tydal

import tydal.schema._

@main def run() = {
//  println(book.name.value)
//  println(book.columns.names)
//  println(book.find("number_of_pages").name.value)

  val query = Select
    .from(book as "b")
    .take($ => (
      $("b", "title") as "btitle",
      $("b", "author_id") as "bauthor"
    ))
    .where(_("bauthor") in
      Select
        .from(author as "a")
        .take(_("a", "id"))
        .where(_("a", "name") like "author?")
    )

  println(Avg(Column["x", nullable[numeric]]).dbType.dbName)
  println(Nullable(Column["x", nullable[numeric]]))

  println(query.from)
  println(query.where)
  println(query.fields)

//  trait StringValue[A]:
//    def value: String
//
//  object StringValue:
//    given[A <: String](using valueOf: ValueOf[A]): StringValue[A] with
//      def value = valueOf.value
//
//  def test[A <: Singleton](a: A)(using s: StringValue[A]): String = s.value
//
//  test("hello")
//

}

/** DbItentifier **/

/** ColumnList **/

//trait ColumnList[T]:
//  def names: List[String]
//
//given ColumnList[EmptyTuple] with
//  def names: List[String] = Nil
//
//given[ColumnName <: String, ColumnType, Tail <: Tuple](using name: ValueOf[ColumnName], tail: ColumnList[Tail]): ColumnList[Column[ColumnName, ColumnType] *: Tail] with
//  def names: List[String] = name.value :: tail.names

/** ColumnFinder **/


/** Usage example **/

enum Colour:
  case red, white

object Colour:
  given Enumerated[Colour] with
    def toString(e: Colour): String = e.toString
    def fromString(s: String): Option[Colour] = scala.util.Try(Colour.valueOf(s)).toOption

object book extends TableSchema[
  "book",
  (
    Column["title", varchar],
    Column["number_of_pages", integer],
    Column["author_id", uuid],
    Column["sleeve_colour", `enum`["colour", Colour]]
  )
]

object author extends TableSchema[
  "author",
  (
    Column["id", uuid],
    Column["name", varchar]
  )
]
