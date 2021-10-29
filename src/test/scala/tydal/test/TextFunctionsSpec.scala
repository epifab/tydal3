package tydal.test

import org.scalatest.Assertion
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import tydal.*

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
        "hello"
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

  "Concat" - {
    "two strings" in {
      testUnique(
        Select(Concat("hello"[text], "world"[text])).compile,
        "SELECT CONCAT($1, $2)",
        "helloworld"
      )
    }

    "string + null" in {
      testUnique(
        Select(Concat("hello"[text], Option.empty[String][nullable[text]])).compile,
        "SELECT CONCAT($1, $2)",
        "hello"
      )
    }

    "null + null" in {
      testUnique(
        Select(Concat(Option("hello")[nullable[text]], Option.empty[String][nullable[text]])).compile,
        "SELECT CONCAT($1, $2)",
        Option("hello")
      )
    }

    "int[] + bool" in {
      testUnique(
        Select(Concat(skunk.data.Arr(3, 4)[array[int4]], true[bool])).compile,
        "SELECT CONCAT($1, $2)",
        "{3,4}t"
      )
    }

    "3 fields" in {
      testUnique(
        Select(Concat(Option(19)[nullable[int4]], Option("yyy")[nullable[text]], 14.6[float8])).compile,
        "SELECT CONCAT($1, $2, $3)",
        "19yyy14.6"
      )
    }

    "4 fields" in {
      testUnique(
        Select(Concat(Option(19)[nullable[int4]], Option("yyy")[nullable[text]], 14.6[float8], "xxx"[varchar])).compile,
        "SELECT CONCAT($1, $2, $3, $4)",
        "19yyy14.6xxx"
      )
    }

    "5 fields" in {
      testUnique(
        Select(Concat(Option(19)[nullable[int4]], Option("yuk")[nullable[text]], 14.6[float8], "kuy"[varchar], List(1,2)[json[List[Int]]])).compile,
        "SELECT CONCAT($1, $2, $3, $4, $5)",
        "19yuk14.6kuy[1,2]"
      )
    }
  }
