package tydal.test.aggregations

import org.scalatest.Assertion
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import skunk.{Query, Void}
import tydal.Schema.artist
import tydal.*
import tydal.compiler.{QueryCompiler, SelectQueryFragment}
import tydal.test.IntegrationTesting

class MinSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Aggregation (MIN)" - {

    "min int2" in {
      testUnique(
        Select(Min(14.toShort[int2])).compile,
        "SELECT MIN($1)",
        Option(14.toShort)
      )
    }

    "min nullable int2" in {
      testUnique(
        Select(Min(Option(14.toShort)[nullable[int2]])).compile,
        "SELECT MIN($1)",
        Option(14.toShort)
      )
    }

    "min int4" in {
      testUnique(
        Select(Min(14[int4])).compile,
        "SELECT MIN($1)",
        Option(14)
      )
    }

    "min int8" in {
      testUnique(
        Select(Min(14.toLong[int8])).compile,
        "SELECT MIN($1)",
        Option(14L)
      )
    }

    "min float4" in {
      testUnique(
        Select(Min(14.2.toFloat[float4])).compile,
        "SELECT MIN($1)",
        Option(14.2.toFloat)
      )
    }

    "min float8" in {
      testUnique(
        Select(Min(14.2[float8])).compile,
        "SELECT MIN($1)",
        Option(14.2)
      )
    }

    "min numeric" in {
      testUnique(
        Select(Min(BigDecimal(14.2)[numeric])).compile,
        "SELECT MIN($1)",
        Option(BigDecimal(14.2))
      )
    }

    "min text" in {
      testUnique(
        Select(Min("hello"[text])).compile,
        "SELECT MIN($1)",
        Option("hello")
      )
    }

    "min varchar" in {
      testUnique(
        Select(Min("hello"[varchar])).compile,
        "SELECT MIN($1)",
        Option("hello")
      )
    }

    "min sized varchar" in {
      testUnique(
        Select(Min("hello"[varcharOf[128]])).compile,
        "SELECT MIN($1)",
        Option("hello")
      )
    }
  }
