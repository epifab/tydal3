package tydal.test.aggregations

import org.scalatest.Assertion
import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.{Query, Void}
import tydal.Schema.artist
import tydal._
import tydal.compiler.{QueryCompiler, SelectQueryFragment}
import tydal.test.IntegrationTesting

class SumSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Aggregation (SUM)" - {

    "sum int2" in {
      testUnique(
        Select(Sum(14.toShort[int2])).compile,
        "SELECT SUM($1)",
        Option(14L)
      )
    }

    "sum optional int2" in {
      testUnique(
        Select(Sum(Option(14.toShort)[nullable[int2]])).compile,
        "SELECT SUM($1)",
        Option(14L)
      )
    }

    "sum int4" in {
      testUnique(
        Select(Sum(14[int4])).compile,
        "SELECT SUM($1)",
        Option(14L)
      )
    }

    "sum int8" in {
      testUnique(
        Select(Sum(14.toLong[int8])).compile,
        "SELECT SUM($1)",
        Option(BigDecimal(14))
      )
    }

    "sum float4" in {
      testUnique(
        Select(Sum(14.2.toFloat[float4])).compile,
        "SELECT SUM($1)",
        Option(14.2.toFloat)
      )
    }

    "sum float8" in {
      testUnique(
        Select(Sum(14.2[float8])).compile,
        "SELECT SUM($1)",
        Option(14.2)
      )
    }

    "sum numeric" in {
      testUnique(
        Select(Sum(BigDecimal(14.2)[numeric])).compile,
        "SELECT SUM($1)",
        Option(BigDecimal(14.2))
      )
    }
  }
