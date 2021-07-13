package tydal.test

import org.scalatest.Assertion
import org.scalatest.freespec._
import org.scalatest.matchers._
import tydal._

class TextFunctionsSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "char_length" - {
    "not null" in {
      testUnique(
        Select(CharLength("hello"[text])).compile,
        "SELECT CHAR_LENGTH($1)",
        5
      )
    }

    "null" in {
      testUnique(
        Select(CharLength(Option.empty[String][nullable[text]])).compile,
        "SELECT CHAR_LENGTH($1)",
        None
      )
    }
  }

  "upper" - {
    "text not null" in {
      testUnique(
        Select(Upper("HELLo"[text])).compile,
        "SELECT UPPER($1)",
        "HELLO"
      )
    }

    "text null" in {
      testUnique(
        Select(Upper(Option.empty[String][nullable[text]])).compile,
        "SELECT UPPER($1)",
        None
      )
    }

    "varchar" in {
      testUnique(
        Select(Upper("HELLo"[varchar])).compile,
        "SELECT UPPER($1)",
        "HELLO"
      )
    }

    "varchar(x)" in {
      testUnique(
        Select(Upper("HELLo"[varcharOf[16]])).compile,
        "SELECT UPPER($1)",
        "HELLO"
      )
    }
  }

  "lower" - {
    "text not null" in {
      testUnique(
        Select(Lower("HELLo"[text])).compile,
        "SELECT LOWER($1)",
        "HELLO"
      )
    }

    "text null" in {
      testUnique(
        Select(Lower(Option.empty[String][nullable[text]])).compile,
        "SELECT LOWER($1)",
        None
      )
    }

    "varchar" in {
      testUnique(
        Select(Lower("HELLo"[varchar])).compile,
        "SELECT LOWER($1)",
        "hello"
      )
    }

    "varchar(x)" in {
      testUnique(
        Select(Lower("HELLo"[varcharOf[16]])).compile,
        "SELECT LOWER($1)",
        "hello"
      )
    }
  }
