package tydal.test

import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.{Query, Void}
import tydal.Schema._
import tydal._
import tydal.compiler._

class SelectQuerySpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Query fields" - {
    "Nothing selected" in {
      testQuery(
        Select.from(artist as "a").compile,
        "SELECT $1 FROM artist a",
        Void
      )
    }

    "One column" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id")).compile,
        "SELECT a.id FROM artist a",
        Void
      )
    }

    "Two columns" in {
      testQuery(
        Select.from(artist as "a").take($ => ($("a", "id"), $("a", "name"))).compile,
        "SELECT a.id, a.name FROM artist a",
        Void
      )
    }

    "Placeholder" in {
      testQuery(
        Select.from(artist as "a").take(_ => Placeholder["hello", varchar]).compile,
        "SELECT $1 FROM artist a",
        "hello" ~~> "blah"
      )
    }

    "Const" in {
      testQuery(
        Select.from(artist as "a").take(_ => 14[int4]).compile,
        "SELECT $1 FROM artist a",
        Void
      )
    }

    "Aggregation (MAX)" - {
      "max int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.toShort[int2])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Short]]
      }

      "max nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Short]]
      }

      "max int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14[int4])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Int]]
      }

      "max int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.toLong[int8])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Long]]
      }

      "max float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.2.toFloat[float4])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Float]]
      }

      "max float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(14.2[float8])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[Double]]
      }

      "max numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max(BigDecimal(14.2)[numeric])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "max text" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[text])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[String]]
      }

      "max varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[varchar])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[String]]
      }

      "max sized varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Max("hello"[varcharOf[128]])).compile,
          "SELECT MAX($1) FROM artist a",
          Void
        ): List[Option[String]]
      }
    }

    "Aggregation (MIN)" - {
      "min int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.toShort[int2])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Short]]
      }

      "min nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Short]]
      }

      "min int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14[int4])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Int]]
      }

      "min int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.toLong[int8])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Long]]
      }

      "min float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.2.toFloat[float4])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Float]]
      }

      "min float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(14.2[float8])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[Double]]
      }

      "min numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min(BigDecimal(14.2)[numeric])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "min text" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[text])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[String]]
      }

      "min varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[varchar])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[String]]
      }

      "min sized varchar" in {
        testQuery(
          Select.from(artist as "a").take(_ => Min("hello"[varcharOf[128]])).compile,
          "SELECT MIN($1) FROM artist a",
          Void
        ): List[Option[String]]
      }
    }

    "Aggregation (AVG)" - {
      "avg int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.toShort[int2])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "avg nullable int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "avg int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14[int4])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "avg int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.toLong[int8])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "avg float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.2.toFloat[float4])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[Double]]
      }

      "avg float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(14.2[float8])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[Double]]
      }

      "avg numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Avg(BigDecimal(14.2)[numeric])).compile,
          "SELECT AVG($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }
    }

    "Aggregation (SUM)" - {
      "sum int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.toShort[int2])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[Long]]
      }

      "sum optional int2" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(Option(14.toShort)[nullable[int2]])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[Long]]
      }

      "sum int4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14[int4])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[Long]]
      }

      "sum int8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.toLong[int8])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }

      "sum float4" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.2.toFloat[float4])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[Float]]
      }

      "sum float8" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(14.2[float8])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[Double]]
      }

      "sum numeric" in {
        testQuery(
          Select.from(artist as "a").take(_ => Sum(BigDecimal(14.2)[numeric])).compile,
          "SELECT SUM($1) FROM artist a",
          Void
        ): List[Option[BigDecimal]]
      }
    }

    "Aggregation (COUNT)" in {
      testQuery(
        Select.from(venue as "v").take(x => Count(x("v", "address"))).compile,
        "SELECT COUNT(v.address) FROM venue v",
        Void
      ): List[Long]
    }

    "Coalesce" in {
      testQuery(
        Select.from(venue as "v").take(x => Coalesce(x("v", "address"), "Somewhere"[text])).compile,
        "SELECT COALESCE(v.address, $1) FROM venue v",
        Void
      ): List[String]
    }

    "Aliased" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "name").as("hello")).compile,
        "SELECT a.name AS hello FROM artist a",
        Void
      )
    }

    "Casted" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id").castTo[varchar]).compile,
        "SELECT a.id::varchar FROM artist a",
        Void
      )
    }

    "Soft cast (nullable)" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "id").nullable).compile,
        "SELECT a.id FROM artist a",
        Void
      )
    }

    "Multiple alias" in {
      testQuery(
        Select.from(artist as "a").take(_("a", "name").as("hello").as("rocky")).compile,
        "SELECT a.name AS rocky FROM artist a",
        Void
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
        Void
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
        Void
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
        Void
      )
    }
  }

  "Binary expression" - {
    "Equals" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") === "?").compile,
        "SELECT $1 FROM artist a WHERE a.name = $2",
        "?" ~~> "foo"
      )
    }

    "Not equals" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") !== "?").compile,
        "SELECT $1 FROM artist a WHERE a.name <> $2",
        "?" ~~> "foo"
      )
    }

    "Greater than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") > "?").compile,
        "SELECT $1 FROM artist a WHERE a.name > $2",
        "?" ~~> "foo"
      )
    }

    "Less than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") < "?").compile,
        "SELECT $1 FROM artist a WHERE a.name < $2",
        "?" ~~> "foo"
      )
    }

    "Greater or equal than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") >= "?").compile,
        "SELECT $1 FROM artist a WHERE a.name >= $2",
        "?" ~~> "foo"
      )
    }

    "Less or equal than" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") <= "?").compile,
        "SELECT $1 FROM artist a WHERE a.name <= $2",
        "?" ~~> "foo"
      )
    }

    "Like" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") like "?").compile,
        "SELECT $1 FROM artist a WHERE a.name LIKE $2",
        "?" ~~> "foo"
      )
    }

    "Case insensitive like" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") ilike "?").compile,
        "SELECT $1 FROM artist a WHERE a.name ILIKE $2",
        "?" ~~> "foo"
      )
    }

    "Is subset" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") subsetOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres <@ $2",
        "?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic)
      )
    }

    "Is superset" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") supersetOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres @> $2",
        "?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic)
      )
    }

    "Overlaps" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "genres") overlaps "?").compile,
        "SELECT $1 FROM artist a WHERE a.genres && $2",
        "?" ~~> skunk.data.Arr(Genre.Rock, Genre.Psychedelic)
      )
    }

    "Is any of" in {
      testQuery(
        Select.from(artist as "a").where(_("a", "name") anyOf "?").compile,
        "SELECT $1 FROM artist a WHERE a.name = ANY($2)",
        "?" ~~> skunk.data.Arr("foo", "bar")
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
        Void
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
        Void
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

  "Group by - having (with UNNEST)" in {
    Select
      .from(artist as "a")
      .groupBy(x => Unnest(x("a", "genres")))
      .take(x => (Unnest(x("a", "genres")) as "genre", Count(x("a", "id"))))
      .having(x => Count(x("a", "id")) > 2[int4])
      .compile
      .sql shouldBe
      "SELECT UNNEST(a.genres) AS genre, COUNT(a.id) FROM artist a GROUP BY UNNEST(a.genres) HAVING COUNT(a.id) > $1"
  }

  "Limit and offset" in {
    testQuery(
      Select.from(artist as "a").offset("offset?").limit("limit?").compile,
      "SELECT $1 FROM artist a OFFSET $2 LIMIT $3",
      ("offset?" ~~> 10000L, "limit?" ~~> 40)
    )
  }

  "Complex query" in {
    testQuery(
      Select
        .from(concert as "c")
        .innerJoin(venue as "v").on(_ ("id") === _ ("c", "venue_id"))
        .innerJoin(concert_artist as "ca").on(_ ("concert_id") === _ ("c", "id"))
        .innerJoin(artist as "a").on(_ ("id") === _ ("ca", "artist_id"))
        .leftJoin(
          Select
            .from(ticket as "t")
            .take(x => (
              x("t", "concert_id") as "cid",
              x("t", "currency") as "currency",
              Min(x("t", "price")) as "min_price"
            ))
            .groupBy(x => (x("cid"), x("currency")))
            .as("tx")
        ).on(_ ("cid") === _ ("c", "id"))
        .take(x => (
          x("c", "begins_at"),
          x("v", "name") as "venue_name",
          x("a", "name") as "artist_name",
          x("tx", "currency"),
          x("tx", "min_price")
        ))
        .where(x => (x("a", "name") anyOf "artists?") or x("tx", "min_price") < "price?")
        .sortBy(x => (x("c", "begins_at"), x("ca", "index")))
        .offset(0L[int8])
        .limit("limit")
        .compile,
      "SELECT c.begins_at, v.name AS venue_name, a.name AS artist_name, tx.currency, tx.min_price" +
        " FROM concert c INNER JOIN venue v ON v.id = c.venue_id" +
        " INNER JOIN concert_artist ca ON ca.concert_id = c.id" +
        " INNER JOIN artist a ON a.id = ca.artist_id" +
        " LEFT JOIN (" +
        "SELECT t.concert_id AS cid, t.currency AS currency, MIN(t.price) AS min_price" +
        " FROM ticket t" +
        " GROUP BY t.concert_id, t.currency" +
        ") tx ON tx.cid = c.id" +
        " WHERE a.name = ANY($1) OR tx.min_price < $2" +
        " ORDER BY c.begins_at, ca.index" +
        " OFFSET $3 LIMIT $4",
      ("artists?" ~~> skunk.data.Arr("Radiohead", "Depeche Mode"), "price?" ~~> Option(BigDecimal(5)), "limit" ~~> 10)
    )
  }

