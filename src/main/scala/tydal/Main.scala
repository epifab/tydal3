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

object book extends Table[
  "book",
  (
    Column["title", String],
    Column["number_of_pages", Int],
    Column["author", String]
  )
]

