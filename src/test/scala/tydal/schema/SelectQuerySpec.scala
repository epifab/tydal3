package tydal.schema

import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.Query
import tydal.schema.compiler._
import tydal.schema.repos.Schema._

class SelectQuerySpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  private def testQuery[A, B](query: Query[A, B], expectedSql: String, input: A): List[B] =
    query.sql shouldBe expectedSql
    session
      .flatMap(_.prepare(query))
      .use(_.stream(input, 4).compile.toList)
      .unsafeRunSync()

  "Query fields" - {
    "Nothing selected" in {
      testQuery(
        Select.from(artist as "a").compile,
        "SELECT $1 FROM artist a",
        EmptyTuple
      )
    }

    "One column" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id")).compile,
        "SELECT a.id FROM artist a",
        EmptyTuple
      )
    }

    "Two columns" in {
      testQuery(
        Select.from(artist as "a").take($ => ($("a", "id"), $("a", "name"))).compile,
        "SELECT a.id, a.name FROM artist a",
        EmptyTuple
      )
    }

    "Placeholder" in {
      testQuery(
        Select.from(artist as "a").take(_ => Placeholder["hello", varchar]).compile,
        "SELECT $1 FROM artist a",
        Tuple("hello" ~~> "blah")
      )
    }

    "Const" in {
      testQuery(
        Select.from(artist as "a").take(_ => 14[int4]).compile,
        "SELECT $1 FROM artist a",
        EmptyTuple
      )
    }

    "Aggregation (MAX)" - {
      "max int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.toShort[int2])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Short] *: EmptyTuple]
      }

      "max nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Short] *: EmptyTuple]
      }

      "max int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14[int4])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Int] *: EmptyTuple]
      }

      "max int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.toLong[int8])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Long] *: EmptyTuple]
      }

      "max float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.2.toFloat[float4])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Float] *: EmptyTuple]
      }

      "max float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.2[float8])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[Double] *: EmptyTuple]
      }

      "max numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(BigDecimal(14.2)[numeric])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "max text" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[text])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }

      "max varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[varchar])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }

      "max sized varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[varcharOf[128]])).compile,
          "SELECT MAX($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }
    }

    "Aggregation (MIN)" - {
      "min int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.toShort[int2])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Short] *: EmptyTuple]
      }

      "min nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Short] *: EmptyTuple]
      }

      "min int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14[int4])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Int] *: EmptyTuple]
      }

      "min int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.toLong[int8])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Long] *: EmptyTuple]
      }

      "min float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.2.toFloat[float4])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Float] *: EmptyTuple]
      }

      "min float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.2[float8])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[Double] *: EmptyTuple]
      }

      "min numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(BigDecimal(14.2)[numeric])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "min text" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[text])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }

      "min varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[varchar])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }

      "min sized varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[varcharOf[128]])).compile,
          "SELECT MIN($1) FROM artist a",
          EmptyTuple
        ): List[Option[String] *: EmptyTuple]
      }
    }

    "Aggregation (AVG)" - {
      "avg int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.toShort[int2])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "avg nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "avg int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14[int4])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "avg int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.toLong[int8])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "avg float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.2.toFloat[float4])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[Double] *: EmptyTuple]
      }

      "avg float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.2[float8])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[Double] *: EmptyTuple]
      }

      "avg numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(BigDecimal(14.2)[numeric])).compile,
          "SELECT AVG($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }
    }

    "Aggregation (SUM)" - {
      "sum int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.toShort[int2])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[Long] *: EmptyTuple]
      }

      "sum optional int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[Long] *: EmptyTuple]
      }

      "sum int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14[int4])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[Long] *: EmptyTuple]
      }

      "sum int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.toLong[int8])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }

      "sum float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.2.toFloat[float4])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[Float] *: EmptyTuple]
      }

      "sum float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.2[float8])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[Double] *: EmptyTuple]
      }

      "sum numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(BigDecimal(14.2)[numeric])).compile,
          "SELECT SUM($1) FROM artist a",
          EmptyTuple
        ): List[Option[BigDecimal] *: EmptyTuple]
      }
    }

    "Coalesce" in {
      testQuery(
        Select.from(venue as "v").take(x => Coalesce(x("v", "address"), "Somewhere"[text])).compile,
        "SELECT COALESCE(v.address, $1) FROM venue v",
        EmptyTuple
      ): List[String *: EmptyTuple]
    }

    "Aliased" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "name").as("hello")).compile,
        "SELECT a.name AS hello FROM artist a",
        EmptyTuple
      )
    }

    "Casted" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id").castTo[varchar]).compile,
        "SELECT a.id::varchar FROM artist a",
        EmptyTuple
      )
    }

    "Soft cast (nullable)" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id").nullable).compile,
        "SELECT a.id FROM artist a",
        EmptyTuple
      )
    }

    "Multiple alias" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "name").as("hello").as("rocky")).compile,
        "SELECT a.name AS rocky FROM artist a",
        EmptyTuple
      )
    }
  }

  "Join" - {
    "Single inner join" in {
      testQuery(
        Select
          .from(concert as "c")
          .innerJoin(venue as "v").on(_("id") === _("c", "venue_id"))
          .compile,
        "SELECT $1 FROM concert c INNER JOIN venue v ON v.id = c.venue_id",
        EmptyTuple
      )
    }

    "Multiple joins" in {
      testQuery(
        Select
          .from(concert as "c")
          .innerJoin(venue as "v").on(_ ("id") === _ ("c", "venue_id"))
          .leftJoin(ticket as "t").on(_ ("concert_id") === _ ("c", "id"))
          .compile,
        "SELECT $1 FROM concert c INNER JOIN venue v ON v.id = c.venue_id LEFT JOIN ticket t ON t.concert_id = c.id",
        EmptyTuple
      )
    }

    "Join subquery" in {
      testQuery(
        Select
          .from(concert as "c")
          .leftJoin(
            Select
              .from(concert as "c2")
              .take(_("c2", "venue_id") as "vid")
              .as("c3")
          ).on(_("vid") === _("c", "venue_id"))
          .compile,
        "SELECT $1 FROM concert c LEFT JOIN (SELECT c2.venue_id AS vid FROM concert c2) c3 ON c3.vid = c.venue_id",
        EmptyTuple
      )
    }
  }

  "Binary expression" - {
    "Equals" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") === "?").compile,
        "SELECT $1 FROM artist a WHERE a.name = $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Not equals" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") !== "?").compile,
        "SELECT $1 FROM artist a WHERE a.name <> $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Greater than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") > "?").compile,
        "SELECT $1 FROM artist a WHERE a.name > $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Less than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") < "?").compile,
        "SELECT $1 FROM artist a WHERE a.name < $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Greater or equal than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") >= "?").compile,
        "SELECT $1 FROM artist a WHERE a.name >= $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Less or equal than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") <= "?").compile,
        "SELECT $1 FROM artist a WHERE a.name <= $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Like" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") like "?").compile,
        "SELECT $1 FROM artist a WHERE a.name LIKE $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Case insensitive like" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") ilike "?").compile,
        "SELECT $1 FROM artist a WHERE a.name ILIKE $2",
        Tuple("?" ~~> "foo")
      )
    }

    "Is subset" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") subsetOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres <@ $2",
        Tuple("?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic))
      )
    }

    "Is superset" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") supersetOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres @> $2",
        Tuple("?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic))
      )
    }

    "Overlaps" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") overlaps "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres && $2",
        Tuple("?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic))
      )
    }

    "Is any of" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") anyOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.name = ANY($2)",
        Tuple("?" ~~> skunk.data.Arr("foo", "bar"))
      )
    }

    "In subquery" in {
      testQuery(
        Select
          .from(artist as "a")
          .where(_("a", "id") in Select
            .from(concert_artist as "ca")
            .take(_("ca", "artist_id") as "aid")
          ).compile,
        "SELECT $1 FROM artist a WHERE a.id IN (SELECT ca.artist_id AS aid FROM concert_artist ca)",
        EmptyTuple
      )
    }

    "Not in subquery" in {
      testQuery(
        Select
          .from(artist as "a")
          .where(_("a", "id") notIn Select
            .from(concert_artist as "ca")
            .take(_("ca", "artist_id") as "aid")
          ).compile,
        "SELECT $1 FROM artist a WHERE a.id NOT IN (SELECT ca.artist_id AS aid FROM concert_artist ca)",
        EmptyTuple
      )
    }
  }

  "Sort by" - {
    "Single field, default order" in {
      Select.from(artist as "a").sortBy(_("a", "name")).compile.sql shouldBe
        "SELECT $1 FROM artist a ORDER BY a.name"
    }

    "Two fields, default and descending order" in {
      Select.from(artist as "a").sortBy($ => (Asc($("a", "name")), Desc($("a", "id")))).compile.sql shouldBe
        "SELECT $1 FROM artist a ORDER BY a.name ASC, a.id DESC"
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
          Min(x("t", "price")) as "lowest_price"
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
