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

object SelectQuerySpec:
  val query =
    Select
      .from(book as "b")
      .innerJoin(author as "a").on(_("id") === _("b", "author_id"))
      .innerJoin(author as "a2").on((a, $) => a("id") === $("b", "author_id") and a("id") === "yo?")
      .take($ => ($("b", "title") as "btitle", $("a", "name")))
      .where(_("btitle") === "title?")
      .where(_("b", "title") === "title?")
      .where($ => Min($("b", "title")) === "title?")
