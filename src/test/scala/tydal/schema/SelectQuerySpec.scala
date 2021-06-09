package tydal.schema

import Tuple.Concat
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

  def compile[From <: Relations, Fields <: Tuple, GroupBy <: Tuple, Where <: LogicalExpr, Having <: LogicalExpr, SortBy <: Tuple, Offset <: Option[Int], Limit <: Option[Int], I1 <: Tuple, I2 <: Tuple](select: SelectQuery[From, Fields, GroupBy, Where, Having, SortBy, Offset, Limit])(
    using
    fields: CommaSeparatedListFragment[FieldFragment, Fields, I1],
    from: RelationFragment[From, I2]
  ): CompiledQueryFragment[I1 Concat I2] =
    fields.build(select.fields).prepend("SELECT ") ++
      from.build(select.from).prepend("FROM ")

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
          .groupBy($ => Tuple($("e1", "student_id")))
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
      // .sortBy(ctx => Descending(ctx("score")) -> Ascending(ctx("sname")))

  // compile(query)
