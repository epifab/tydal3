package tydal.test

import cats.effect.IO
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import skunk.Query
import skunk.data.Arr
import skunk.syntax.all.*
import tydal.*
import tydal.Schema.*

import java.util.UUID

class FilterSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:
  val insert =
    Insert
      .into(artist)
      .fields(a => (a("id"), a("name"), a("genres")))
      .compile

  val select: Query[("n" ~~> String, "g1" ~~> Genre, "g2" ~~> Genre), String] =
    Select
      .from(artist as "a")
      .take(_("name"))
      .where { x =>
        (x("name") like "n") and <<((x("a", "genres") contains "g1") or (x("a", "genres") contains "g2"))
      }.compile

  "A where clause respects precedence" in {
    session.use { s =>
      for {
        _ <- s.execute(sql"TRUNCATE TABLE artist CASCADE".command)
        _ <- s.prepareR(insert).use { statement =>
          for {
            _ <- statement.execute(("id" ~~> UUID.randomUUID(), "name" ~~> "Led Zeppelin", "genres" ~~> Arr(Genre.Rock)))
            _ <- statement.execute(("id" ~~> UUID.randomUUID(), "name" ~~> "Metallica", "genres" ~~> Arr(Genre.Metal)))
          } yield ()
        }
        artist <- s.prepareR(select).use(_.unique(("n" ~~> "%Metal%", "g1" ~~> Genre.Metal, "g2" ~~> Genre.Rock)))
      } yield (artist shouldBe "Metallica")
      // this is testing the accuracy of a AND (b OR c)
      // if there were no parentheses, it'd return both artists
    }.unsafeRunSync()
  }
