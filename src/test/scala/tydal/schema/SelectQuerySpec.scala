package tydal.schema

import tydal.schema.compiler._

object students extends TableSchema[
  "students",
  (
    "id" :=: uuid,
    "name" :=: varchar,
    "date_of_birth" :=: date
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

object SelectQuerySpec:

  Select.from(students as "s").compile
  Select.from(students as "s").take(_("s", "id")).compile
  Select.from(students as "s").take($ => ($("s", "id"), $("s", "name"))).compile
  Select.from(students as "s").take($ => ($("s", "id"), Max($("s", "name")))).compile
  Select.from(students as "s").take($ => ($("s", "id"), Distinct($("s", "name")) as "sname")).compile
  Select.from(students as "s").take(_("s", "id")).where(_("s", "name") === "name?").compile
  Select.from(students as "s").take(_("s", "id")).where($ => ($("s", "name") === "name?") or ($("s", "id") === "ids?")).compile
  Select.from(students as "s").take(_("s", "id")).sortBy(_("s", "name")).compile
  Select.from(students as "s").take(_("s", "id")).sortBy($ => ($("s", "name"), Desc($("s", "id")))).compile
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
