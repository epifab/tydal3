package tydal

import tydal.schema.DbType.Enumerated
import tydal.schema._

@main def run() = {
//  println(book.name.value)
//  println(book.columns.names)
//  println(book.find("number_of_pages").name.value)

  val query = Select
    .from(book as "b")
    .take($ => (
      $("b", "title") as "btitle",
      $("b", "author") as "bauthor"
    ))

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

given Enumerated[Colour] with
  def toString(e: Colour): String = e.toString
  def fromString(s: String): Colour = Colour.valueOf(s)

object book extends Table[
  "book",
  (
    Column["title", DbType.varchar],
    Column["number_of_pages", DbType.integer],
    Column["author", DbType.varchar],
    Column["sleeve_colour", DbType.`enum`["colour", Colour]]
  )
]

