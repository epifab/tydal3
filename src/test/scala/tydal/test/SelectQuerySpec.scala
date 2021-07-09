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
        Select.from(artist as "a").take(x => (x("a", "id"), x("a", "name"))).compile,
        "SELECT a.id, a.name FROM artist a",
        Void
      )
    }

    "Placeholder" in {
      testQuery(
        Select(Placeholder["hello", varchar]).compile,
        "SELECT $1",
        "hello" ~~> "blah"
      ) shouldBe List("blah")
    }

    "Const" in {
      testQuery(
        Select(14[int4]).compile,
        "SELECT $1",
        Void
      ) shouldBe List(14)
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

  "Union" - {
    "union" in {
      testQuery(
        (Select(4[int4]) union Select(5[int4]) union Select(5[int4])).compile,
        "SELECT $1 UNION SELECT $2 UNION SELECT $3",
        Void
      ).toSet shouldBe Set(4, 5)
    }

    "union all" in {
      testQuery(
        (Select(4[int4]) union Select(5[int4]) unionAll Select(5[int4])).compile,
        "SELECT $1 UNION SELECT $2 UNION ALL SELECT $3",
        Void
      ).toSet shouldBe Set(4, 5, 5)
    }

  }
