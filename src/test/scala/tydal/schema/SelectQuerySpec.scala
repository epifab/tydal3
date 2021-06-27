package tydal.schema

import tydal.schema.compiler._
import org.scalatest.freespec._
import org.scalatest.matchers._

enum Interest:
  case music, art, history, math

object Interest:
  given Enumerated[Interest] with
    def toString(interest: Interest): String = interest.toString
    def fromString(s: String): Option[Interest] = Some(Interest.valueOf(s))

object students extends TableSchema[
  "students",
  (
    "id" :=: uuid,
    "name" :=: varchar,
    "email" :=: nullable[varchar],
    "date_of_birth" :=: date,
    "interests" :=: array[`enum`["interest", Interest]]
  )
]

object courses extends TableSchema[
  "courses",
  (
    "id" :=: uuid,
    "name" :=: varchar
  )
]

object exams extends TableSchema[
  "exams",
  (
    "student_id" :=: uuid,
    "course_id" :=: uuid,
    "registered_on" :=: timestamp,
    "score" :=: integer
  )
]


class SelectQuerySpec extends AnyFreeSpec with should.Matchers:

  "Nothing selected" in {
    Select.from(students as "s").compile.sql shouldBe
      "SELECT 1 FROM students s"
  }

  "One column" in {
    Select.from(students as "s").take(_("s", "id")).compile.sql shouldBe
      "SELECT s.id FROM students s"
  }

  "Two columns" in {
    Select.from(students as "s").take($ => ($("s", "id"), $("s", "name"))).compile.sql shouldBe
      "SELECT s.id, s.name FROM students s"
  }

  "Placeholder" in {
    Select.from(students as "s").take(_ => "hello".placeholder[varchar]).compile.sql shouldBe
      "SELECT ?::varchar FROM students s"
  }

  "Literal" in {
    Select.from(students as "s").take(_ => 14.literal[integer]).compile.sql shouldBe
      "SELECT ?::integer FROM students s"
  }

  "Aggregate" in {
    Select.from(students as "s").take($ => Max($("s", "name"))).compile.sql shouldBe
      "SELECT MAX(s.name) FROM students s"
  }

  "Aliased" in {
    Select.from(students as "s").take(_("s", "name").as("hello")).compile.sql shouldBe
      "SELECT s.name AS hello FROM students s"
  }

  "Casted" in {
    Select.from(students as "s").take(_("s", "id").castTo[varchar]).compile.sql shouldBe
      "SELECT s.id::varchar FROM students s"
  }

  "Soft cast (nullable)" in {
    Select.from(students as "s").take(_("s", "id").nullable).compile.sql shouldBe
      "SELECT s.id FROM students s"
  }

  "Multiple alias" in {
    Select.from(students as "s").take(_("s", "name").as("hello").as("rocky")).compile.sql shouldBe
      "SELECT s.name AS rocky FROM students s"
  }

  Select.from(students as "s").where(_("s", "name") === "name?").compile
  Select.from(students as "s").where(_("s", "name") === "yo".literal[varchar]).compile
  Select.from(students as "s").where($ => ($("s", "name") === "name?") or ($("s", "id") === "ids?")).compile
  Select.from(students as "s").sortBy(_("s", "name")).compile
  Select.from(students as "s").sortBy($ => ($("s", "name"), Desc($("s", "id")))).compile
  Select.from(students as "s").innerJoin(exams as "e").on(_("student_id") === _("s", "id")).compile
  Select.from(students as "s").innerJoin(exams as "e").on(_("student_id") === _("s", "id")).compile

  Select.from(students as "s")
    .leftJoin(exams as "e").on(_("student_id") === _("s", "id"))
    .innerJoin(courses as "c").on(_("id") === _("e", "course_id"))
    .compile

  Select.from(students as "s")
    .innerJoin(
      Select
        .from(exams as "ee")
        .take(_("ee", "student_id") as "sid")
        .as("e")
    ).on(_("sid") === _("s", "id"))
    .compile

  val query =
    Select
      .from(students as "s")
      .innerJoin(
        // max score per student
        Select
          .from(exams as "e1")
          .take(ctx => (
            ctx("e1", "student_id") as "sid",
            Max(ctx("e1", "score")) as "score"
          ))
          .where(_("e1", "registered_on") > "exam_min_date?")
          .groupBy(_("e1", "student_id"))
          .as("me1")
      )
      .on(_("sid") === _("s", "id"))
      .innerJoin(
        // select only the latest exam
        Select
          .from(exams as "e2")
          .take(ctx => (
            ctx("e2", "student_id")          as "sid",
            ctx("e2", "score")               as "score",
            Max(ctx("e2", "registered_on"))  as "etime"
          ))
          .groupBy(ctx => (ctx("e2", "student_id"), ctx("e2", "score")))
          .as("me2")
      )
      .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
      .innerJoin(exams as "e")
      .on((e, ctx) => e("registered_on") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
      .innerJoin(courses as "c")
      .on(_("id") === _("e", "course_id"))
      .take(ctx => (
        ctx("s", "id")             as "sid",
        ctx("s", "name")           as "sname",
        ctx("e", "score")          as "score",
        ctx("e", "registered_on")  as "etime",
        ctx("c", "name")           as "cname"
      ))
      .where(ctx => ctx("s", "date_of_birth") > "student_min_dob?" and (ctx("s", "date_of_birth") < "student_max_dob?"))
      .sortBy(ctx => (ctx("score").desc, ctx("sname")))
      .inRange(0, 100)
      .compile
