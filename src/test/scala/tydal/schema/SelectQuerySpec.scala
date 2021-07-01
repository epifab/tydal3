package tydal.schema

import org.scalatest.freespec._
import org.scalatest.matchers._
import tydal.schema.repos.Schema._
import tydal.schema.compiler._

class SelectQuerySpec extends AnyFreeSpec with should.Matchers:

  "Query fields" - {
    "Nothing selected" in {
      Select.from(artist as "a").compile.sql shouldBe
        "SELECT 1 FROM artist a"
    }

    "One column" in {
      Select.from(artist as "a").take(_("a", "id")).compile.sql shouldBe
        "SELECT a.id FROM artist a"
    }

    "Two columns" in {
      Select.from(artist as "a").take($ => ($("a", "id"), $("a", "name"))).compile.sql shouldBe
        "SELECT a.id, a.name FROM artist a"
    }

    "Placeholder" in {
      Select.from(artist as "a").take(_ => Placeholder["hello", varchar]).compile.sql shouldBe
        "SELECT $1 FROM artist a"
    }

    "Const" in {
      Select.from(artist as "a").take(_ => 14[int4]).compile.sql shouldBe
        "SELECT $1 FROM artist a"
    }

    "Aggregate" in {
      Select.from(artist as "a").take($ => Max($("a", "name"))).compile.sql shouldBe
        "SELECT MAX(a.name) FROM artist a"
    }

    "Aliased" in {
      Select.from(artist as "a").take(_("a", "name").as("hello")).compile.sql shouldBe
        "SELECT a.name AS hello FROM artist a"
    }

    "Casted" in {
      Select.from(artist as "a").take(_("a", "id").castTo[varchar]).compile.sql shouldBe
        "SELECT a.id::varchar FROM artist a"
    }

    "Soft cast (nullable)" in {
      Select.from(artist as "a").take(_("a", "id").nullable).compile.sql shouldBe
        "SELECT a.id FROM artist a"
    }

    "Multiple alias" in {
      Select.from(artist as "a").take(_("a", "name").as("hello").as("rocky")).compile.sql shouldBe
        "SELECT a.name AS rocky FROM artist a"
    }
  }

  "Join" - {
    "Single inner join" in {
      Select
        .from(concert as "c")
        .innerJoin(venue as "v").on(_("id") === _("c", "venue_id"))
        .compile
        .sql shouldBe "SELECT 1 FROM concert c INNER JOIN venue v ON v.id = c.venue_id"
    }

    "Multiple joins" in {
      val sql = Select
        .from(concert as "c")
        .innerJoin(venue as "v").on(_ ("id") === _ ("c", "venue_id"))
        .leftJoin(ticket as "t").on(_ ("concert_id") === _ ("c", "id"))
        .compile
        .sql
      println(sql)
      sql shouldBe "SELECT 1 FROM concert c INNER JOIN venue v ON v.id = c.venue_id LEFT JOIN ticket t ON t.concert_id = c.id"
    }

    "Join subquery" in {
      Select
        .from(concert as "c")
        .leftJoin(
          Select
            .from(concert as "c2")
            .take(_("c2", "venue_id") as "vid")
            .as("c3")
        ).on(_("vid") === _("c", "venue_id"))
        .compile
        .sql shouldBe "SELECT 1 FROM concert c LEFT JOIN (SELECT c2.venue_id AS vid FROM concert c2) c3 ON c3.vid = c.venue_id"
    }
  }

  "Binary expression" - {
    "Equals" in {
      Select.from(artist as "a").where(_("a", "name") === "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name = $1"
    }

    "Not equals" in {
      Select.from(artist as "a").where(_("a", "name") !== "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name <> $1"
    }

    "Greater than" in {
      Select.from(artist as "a").where(_("a", "name") > "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name > $1"
    }

    "Less than" in {
      Select.from(artist as "a").where(_("a", "name") < "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name < $1"
    }

    "Greater or equal than" in {
      Select.from(artist as "a").where(_("a", "name") >= "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name >= $1"
    }

    "Less or equal than" in {
      Select.from(artist as "a").where(_("a", "name") <= "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name <= $1"
    }

    "Like" in {
      Select.from(artist as "a").where(_("a", "name") like "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name LIKE $1"
    }

    "Case insensitive like" in {
      Select.from(artist as "a").where(_("a", "name") ilike "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name ILIKE $1"
    }

    "Is subset" in {
      Select.from(artist as "a").where(_("a", "genres") subsetOf "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.genres <@ $1"
    }

    "Is superset" in {
      Select.from(artist as "a").where(_("a", "genres") supersetOf "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.genres @> $1"
    }

    "Overlaps" in {
      Select.from(artist as "a").where(_("a", "genres") overlaps "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.genres && $1"
    }

    "Is any of" in {
      Select.from(artist as "a").where(_("a", "name") anyOf "?").compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.name = ANY($1)"
    }

    "In subquery" in {
      Select
        .from(artist as "a")
        .where(_("a", "id") in Select
          .from(concert_artist as "ca")
          .take(_("ca", "artist_id") as "aid")
        ).compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.id IN (SELECT ca.artist_id AS aid FROM concert_artist ca)"
    }

    "Not in subquery" in {
      Select
        .from(artist as "a")
        .where(_("a", "id") notIn Select
          .from(concert_artist as "ca")
          .take(_("ca", "artist_id") as "aid")
        ).compile.sql shouldBe
        "SELECT 1 FROM artist a WHERE a.id NOT IN (SELECT ca.artist_id AS aid FROM concert_artist ca)"
    }
  }

  "Sort by" - {
    "Single field, default order" in {
      Select.from(artist as "a").sortBy(_("a", "name")).compile.sql shouldBe
        "SELECT 1 FROM artist a ORDER BY a.name"
    }

    "Two fields, default and descending order" in {
      Select.from(artist as "a").sortBy($ => (Asc($("a", "name")), Desc($("a", "id")))).compile.sql shouldBe
        "SELECT 1 FROM artist a ORDER BY a.name ASC, a.id DESC"
    }
  }

  "Group by - having" in {
    Select
      .from(artist as "a")
      .groupBy(x => Unnest(x("a", "genres")))
      .take(x => (Unnest(x("a", "genres")) as "genre", Count(x("a", "id"))))
      .having(x => Count(x("a", "id")) > 2[int4])
      .compile
      .sql shouldBe
      "SELECT UNNEST(a.genres) AS genre, COUNT(a.id) FROM artist a GROUP BY UNNEST(a.genres) HAVING COUNT(a.id) > $1"
  }

  Select.from(artist as "a")
    .innerJoin(
      Select
        .from(concert_artist as "ca")
        .innerJoin(ticket as "t").on(_("concert_id") === _("ca", "concert_id"))
        .take(x => (
          x("ca", "artist_id") as "artist_id",
          x("t", "price").min as "lowest_price"
        ))
        .as("tx")
    ).on(_("artist_id") === _("a", "id"))
    .compile

//  val query =
//    Select
//      .from(artist as "a")
//      .innerJoin(
//        // max score per student
//        Select
//          .from(exams as "e1")
//          .take(ctx => (
//            ctx("e1", "student_id") as "sid",
//            Max(ctx("e1", "score")) as "score"
//          ))
//          .where(_("e1", "registered_on") > "exam_min_date?")
//          .groupBy(_("e1", "student_id"))
//          .as("me1")
//      )
//      .on(_("sid") === _("s", "id"))
//      .innerJoin(
//        // select only the latest exam
//        Select
//          .from(exams as "e2")
//          .take(ctx => (
//            ctx("e2", "student_id")          as "sid",
//            ctx("e2", "score")               as "score",
//            Max(ctx("e2", "registered_on"))  as "etime"
//          ))
//          .groupBy(ctx => (ctx("e2", "student_id"), ctx("e2", "score")))
//          .as("me2")
//      )
//      .on((me2, ctx) => me2("sid") === ctx("me1", "sid") and (me2("score") === ctx("me1", "score")))
//      .innerJoin(exams as "e")
//      .on((e, ctx) => e("registered_on") === ctx("me2", "etime") and (e("student_id") === ctx("me2", "sid")))
//      .innerJoin(courses as "c")
//      .on(_("id") === _("e", "course_id"))
//      .take(ctx => (
//        ctx("s", "id")             as "sid",
//        ctx("s", "name")           as "sname",
//        ctx("e", "score")          as "score",
//        ctx("e", "registered_on")  as "etime",
//        ctx("c", "name")           as "cname"
//      ))
//      .where(ctx => ctx("s", "date_of_birth") > "student_min_dob?" and (ctx("s", "date_of_birth") < "student_max_dob?"))
//      .sortBy(ctx => (ctx("score").desc, ctx("sname")))
//      .inRange(0, 100)
//      .compile
