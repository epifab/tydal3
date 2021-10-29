package tydal.test.aggregations

import org.scalatest.freespec.*
import org.scalatest.matchers.*
import skunk.{Query, Void}
import tydal.Schema.*
import tydal.*
import tydal.compiler.*
import tydal.test.IntegrationTesting

class AvgSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Aggregation (AVG)" - {
    "avg int2" in {
      testUnique(
        Select(Avg(14.toShort[int2])).compile,
        "SELECT AVG($1)",
        Option(BigDecimal(14))
      )
    }

    "avg nullable int2" in {
      testUnique(
        Select(Avg(Option(14.toShort)[nullable[int2]])).compile,
        "SELECT AVG($1)",
        Option(BigDecimal(14))
      )
    }

    "avg int4" in {
      testUnique(
        Select(Avg(14[int4])).compile,
        "SELECT AVG($1)",
        Option(BigDecimal(14))
      )
    }

    "avg int8" in {
      testUnique(
        Select(Avg(14.toLong[int8])).compile,
        "SELECT AVG($1)",
        Option(14.toLong)
      )
    }

    // There are some rounding problems for the value returned
    "avg float4" ignore {
      testUnique(
        Select(Avg(14.2.toFloat[float4])).compile,
        "SELECT AVG($1)",
        Option(14.2)
      )
    }

    "avg float8" in {
      testUnique(
        Select(Avg(14.2[float8])).compile,
        "SELECT AVG($1)",
        Option(14.2)
      )
    }

    "avg numeric" in {
      testUnique(
        Select(Avg(BigDecimal(14.2)[numeric])).compile,
        "SELECT AVG($1)",
        Option(BigDecimal(14.2))
      )
    }
  }
