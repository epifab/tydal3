package tydal.schema

import tydal.schema.compiler._
import org.scalatest.freespec._
import org.scalatest.matchers._
import java.util.UUID

object InsertCommandSpec extends App:  //  AnyFreeSpec with should.Matchers:

  val command =
    Insert
      .into(students)
      .fields($ => ($("id"), $("name"), $("email")))
      .compile

  // todo: yet another scala bug: if I move the type to command directly it doesn't compile
  val y: skunk.Command[("id" ~~> UUID, "name" ~~> String, "email" ~~> Option[String])] = command
  println(y.sql)
