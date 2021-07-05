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

  "Insert command" in {
    testCommand(
      Insert
        .into(Schema.artist)
        .fields(x => (x("id"), x("name"), x("genres")))
        .compile,
      "INSERT INTO artist (id, name, genres) VALUES ($1, $2, $3)",
      (
        "id" ~~> UUID.randomUUID(),
        "name" ~~> "The Doors",
        "genres" ~~> skunk.data.Arr(Schema.Genre.Psychedelic, Schema.Genre.Rock)
      )
    )
  }

  "Update command" in {
    testCommand(
      Update(Schema.artist)
        .fields(x => (x("name"), x("genres")))
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
