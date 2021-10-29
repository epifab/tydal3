package tydal.test.aggregations

import org.scalatest.Assertion
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import skunk.{Query, Void}
import tydal.Schema.artist
import tydal.*
import tydal.compiler.{QueryCompiler, SelectQueryFragment}
import tydal.test.IntegrationTesting

class MaxSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Aggregation (MAX)" - {

    "max int2 from union" in {
      testUnique(
        Select.from(
          (Select(5.toShort[int2] as "bar") union
            Select(10.toShort[int2]) union
            Select(6.toShort[int2])).as("foo")
        ).take(x => Max(x("foo", "bar"))).compile,
        "SELECT MAX(foo.bar) FROM (SELECT $1 AS bar UNION SELECT $2 UNION SELECT $3) foo",
        Option(10.toShort)
      )
    }

    "max nullable int2" in {
      testUnique(
        Select(Max(Option(14.toShort)[nullable[int2]])).compile,
        "SELECT MAX($1)",
        Option(14.toShort)
      )
    }

    "max int4" in {
      testUnique(
        Select(Max(14[int4])).compile,
        "SELECT MAX($1)",
        Option(14)
      )
    }

    "max int8" in {
      testUnique(
        Select(Max(14.toLong[int8])).compile,
        "SELECT MAX($1)",
        Option(14L)
      )
    }

    "max float4" in {
      testUnique(
        Select(Max(14.2.toFloat[float4])).compile,
        "SELECT MAX($1)",
        Option(14.2.toFloat)
      )
    }

    "max float8" in {
      testUnique(
        Select(Max(14.2[float8])).compile,
        "SELECT MAX($1)",
        Option(14.2)
      )
    }

    "max numeric" in {
      testUnique(
        Select(Max(BigDecimal(14.2)[numeric])).compile,
        "SELECT MAX($1)",
        Option(BigDecimal(14.2))
      )
    }

    "max text" in {
      testUnique(
        Select(Max("hello"[text])).compile,
        "SELECT MAX($1)",
        Option("hello")
      )
    }

    "max varchar" in {
      testUnique(
        Select(Max("hello"[varchar])).compile,
        "SELECT MAX($1)",
        Option("hello")
      )
    }

    "max sized varchar" in {
      testUnique(
        Select(Max("hello"[varcharOf[128]])).compile,
        "SELECT MAX($1)",
        Option("hello")
      )
    }
  }
