package tydal.schema

object book extends TableSchema[
  "book",
  (
    "title" :=: varchar,
    "number_of_pages" :=: integer,
    "body" :=: text,
    "author_id" :=: uuid
  )
]

object author extends TableSchema[
  "author",
  (
    "id" :=: uuid,
    "name" :=: varchar
  )
]

object FinderSpec:
  val query = Select
    .from(book as "b")
    .take($ => ($("b", "title") as "btitle", $("b", "author_id")))
    .where(_("btitle") === "title?")
    .where(_("b", "title") === "title?")
    .where($ => Min($("b", "title")) === "title?")
