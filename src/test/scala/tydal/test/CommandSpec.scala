package tydal.test

import org.scalatest.freespec._
import org.scalatest.matchers._
import tydal._
import tydal.compiler._
import skunk.Command

import java.util.UUID

class CommandSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  private def testCommand[A](command: Command[A], expectedSql: String, input: A): Unit =
    command.sql shouldBe expectedSql
    session
      .flatMap(_.prepare(command))
      .use(_.execute(input))
      .unsafeRunSync()

  "Insert command" - {
    "Simple INSERT" in {
      testCommand(
        Insert
          .into(Schema.artist)
          .fields(a => (a("id"), a("name"), a("genres")))
          .compile,
        "INSERT INTO artist (id, name, genres) VALUES ($1, $2, $3)",
        (
          "id" ~~> UUID.randomUUID(),
          "name" ~~> "The Doors",
          "genres" ~~> skunk.data.Arr(Schema.Genre.Psychedelic, Schema.Genre.Rock)
        )
      )
    }

    "On conflict do nothing" in {
      testCommand(
        Insert
          .into(Schema.artist)
          .fields(a => (a("id"), a("name"), a("genres")))
          .onConflict(_("id"))
          .doNothing
          .compile,
        "INSERT INTO artist (id, name, genres) VALUES ($1, $2, $3) ON CONFLICT (id) DO NOTHING",
        (
          "id" ~~> UUID.randomUUID(),
          "name" ~~> "The Doors",
          "genres" ~~> skunk.data.Arr(Schema.Genre.Psychedelic, Schema.Genre.Rock)
        )
      )
    }

    "On conflict do update" in {
      testCommand(
        Insert
          .into(Schema.artist)
          .fields(a => (a("id"), a("name"), a("genres")))
          .onConflict(_("id"))
          .doUpdate(a => a("name") :== Placeholder["newName", varchar])
          .compile,
        "INSERT INTO artist (id, name, genres) VALUES ($1, $2, $3) ON CONFLICT (id) DO UPDATE SET name = $4",
        (
          "id" ~~> UUID.randomUUID(),
          "name" ~~> "The Doors",
          "genres" ~~> skunk.data.Arr(Schema.Genre.Psychedelic, Schema.Genre.Rock),
          "newName" ~~> "The Doors"
        )
      )
    }
  }

  "Update command" - {

    "with a simple list of fields" in {
      testCommand(
        Update(Schema.artist)
          .set(a => (a("name"), a("genres")))
          .where(_("id") === "id?")
          .compile,
        "UPDATE artist SET name = $1, genres = $2 WHERE id = $3",
        (
          "name" ~~> "The Doors",
          "genres" ~~> skunk.data.Arr(Schema.Genre.Psychedelic, Schema.Genre.Rock),
          "id?" ~~> UUID.fromString("3fb2bd37-fc4a-4c9f-96bc-a4c253bc5857")
        )
      )
    }

    "with set column = expr of the same type" in {
      testCommand(
        Update(Schema.ticket)
          .set(t => t("price") :== t("price") + (t("price") * Placeholder["factor?", float8]))
          .where(_("id") === "id?")
          .compile,
        "UPDATE ticket SET price = (price + (price * $1)) WHERE id = $2",
        (
          "factor?" ~~> -0.2,  // 20% discount
          "id?" ~~> UUID.fromString("3fb2bd37-fc4a-4c9f-96bc-a4c253bc5857")
        )
      )
    }

    "with set column = expr of a comparable type" in {
      testCommand(
        Update(Schema.ticket)
          .set(t => t("price") :== Placeholder["newPrice?", float8])
          .where(_("id") === "id?")
          .compile,
        "UPDATE ticket SET price = $1 WHERE id = $2",
        (
          "newPrice?" ~~> 15.99,  // 20% discount
          "id?" ~~> UUID.fromString("3fb2bd37-fc4a-4c9f-96bc-a4c253bc5857")
        )
      )
    }

  }

  "Delete command" in {
    testCommand(
      Delete
        .from(Schema.ticket)
        .where(_("price") > 50[int4])
        .compile,
      "DELETE FROM ticket WHERE price > $1",
      skunk.Void
    )
  }