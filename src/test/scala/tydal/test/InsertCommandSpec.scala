package tydal.test

import org.scalatest.freespec._
import org.scalatest.matchers._
import tydal._
import tydal.compiler._

import java.util.UUID

class InsertCommandSpec extends AnyFreeSpec with should.Matchers:

  val command =
    Insert
      .into(Schema.artist)
      .fields($ => ($("id"), $("name"), $("genres")))
      .compile

  // todo: yet another scala bug: if I move the type to command directly it doesn't compile
  val y: skunk.Command[("id" ~~> UUID, "name" ~~> String, "genres" ~~> skunk.data.Arr[Schema.Genre])] = command

  "A basic SQL command" in {
    command.sql shouldBe "INSERT INTO artist (id, name, genres) VALUES ($1, $2, $3)"
  }
