package tydal.test.arithmetics

import cats.kernel.Eq
import org.scalatest.Assertion
import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.Void
import tydal._
import tydal.test.IntegrationTesting

class DivSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  def testDiv[A: IsNumerical, B: IsNumerical, C, Out](a: Const[A], b: Const[B], result: Out)(
    using
    ArithmeticType[A, B, C],
    DbType.Aux[C, Out],
    Eq[Out]
  ): Assertion =
    testUnique(
      Select(a / b).compile,
      "SELECT $1 / $2",
      result
    )

  "int2 / int4" in testDiv(2.toShort[int2], 2[int4], 1)
  "int2 / int2" in testDiv(2.toShort[int2], 2.toShort[int2], 1.toShort)
  "int2 / int8" in testDiv(2.toShort[int2], 2L[int8], 1L)
  "int2 / float4" in testDiv(2.toShort[int2], 2.toFloat[float4], 1.toDouble)
  "int2 / float8" in testDiv(2.toShort[int2], 2.toDouble[float8], 1.toDouble)
  "int2 / numeric" in testDiv(2.toShort[int2], BigDecimal(2)[numeric], BigDecimal(1))
  "int2 / numeric(p,s)" in testDiv(2.toShort[int2], BigDecimal(2)[numericOf[4, 2]], BigDecimal(1))

  "int4 / int2" in testDiv(2[int4], 2.toShort[int2], 1)
  "int4 / int4" in testDiv(2[int4], 2[int4], 1)
  "int4 / int8" in testDiv(2[int4], 2L[int8], 1L)
  "int4 / float4" in testDiv(2[int4], 2.toFloat[float4], 1.0)
  "int4 / float8" in testDiv(2[int4], 2.toDouble[float8], 1.0)
  "int4 / numeric" in testDiv(2[int4], BigDecimal(2)[numeric], BigDecimal(1))
  "int4 / numeric(p,s)" in testDiv(2[int4], BigDecimal(2)[numericOf[4, 2]], BigDecimal(1))

  "int8 / int2" in testDiv(2L[int8], 2.toShort[int2], 1L)
  "int8 / int4" in testDiv(2L[int8], 2[int4], 1L)
  "int8 / int8" in testDiv(2L[int8], 2L[int8], 1L)
  "int8 / float4" in testDiv(2L[int8], 2.toFloat[float4], 1.0)
  "int8 / float8" in testDiv(2L[int8], 2.toDouble[float8], 1.0)
  "int8 / numeric" in testDiv(2L[int8], BigDecimal(2)[numeric], BigDecimal(1))
  "int8 / numeric(p,s)" in testDiv(2L[int8], BigDecimal(2)[numericOf[4, 2]], BigDecimal(1))

  "float4 / int2" in testDiv(2.toFloat[float4], 2.toShort[int2], 1.0)
  "float4 / int4" in testDiv(2.toFloat[float4], 2[int4], 1.0)
  "float4 / int8" in testDiv(2.toFloat[float4], 2L[int8], 1.0)
  "float4 / float4" in testDiv(2.toFloat[float4], 2.toFloat[float4], 1.toFloat)
  "float4 / float8" in testDiv(2.toFloat[float4], 2.toDouble[float8], 1.0)
  "float4 / numeric" in testDiv(2.toFloat[float4], BigDecimal(2)[numeric], 1.0)
  "float4 / numeric(p,s)" in testDiv(2.toFloat[float4], BigDecimal(2)[numericOf[4, 2]], 1.0)

  "float8 / int2" in testDiv(2.toDouble[float8], 2.toShort[int2], 1.0)
  "float8 / int4" in testDiv(2.toDouble[float8], 2[int4], 1.0)
  "float8 / int8" in testDiv(2.toDouble[float8], 2L[int8], 1.0)
  "float8 / float4" in testDiv(2.toDouble[float8], 2.toFloat[float4], 1.0)
  "float8 / float8" in testDiv(2.toDouble[float8], 2.toDouble[float8], 1.0)
  "float8 / numeric" in testDiv(2.toDouble[float8], BigDecimal(2)[numeric], 1.0)
  "float8 / numeric(p,s)" in testDiv(2.toDouble[float8], BigDecimal(2)[numericOf[4, 2]], 1.0)

  "numeric / int2" in testDiv(BigDecimal(2)[numeric], 2.toShort[int2], BigDecimal(1))
  "numeric / int4" in testDiv(BigDecimal(2)[numeric], 2[int4], BigDecimal(1.0))
  "numeric / int8" in testDiv(BigDecimal(2)[numeric], 2L[int8], BigDecimal(1.0))
  "numeric / float4" in testDiv(BigDecimal(2)[numeric], 2.toFloat[float4], 1.0)
  "numeric / float8" in testDiv(BigDecimal(2)[numeric], 2.toDouble[float8], 1.0)
  "numeric / numeric" in testDiv(BigDecimal(2)[numeric], BigDecimal(2)[numeric], BigDecimal(1))
  "numeric / numeric(p,s)" in testDiv(BigDecimal(2)[numeric], BigDecimal(2)[numericOf[4, 2]], BigDecimal(1))

  "numeric(p,s) / int2" in testDiv(BigDecimal(2)[numeric], 2.toShort[int2], BigDecimal(1))
  "numeric(p,s) / int4" in testDiv(BigDecimal(2)[numeric], 2[int4], BigDecimal(1))
  "numeric(p,s) / int8" in testDiv(BigDecimal(2)[numeric], 2L[int8], BigDecimal(1))
  "numeric(p,s) / float4" in testDiv(BigDecimal(2)[numeric], 2.toFloat[float4], 1.0)
  "numeric(p,s) / float8" in testDiv(BigDecimal(2)[numeric], 2.toDouble[float8], 1.0)
  "numeric(p,s) / numeric" in testDiv(BigDecimal(2)[numeric], BigDecimal(2)[numericOf[4, 4]], BigDecimal(1))
  "numeric(p1,s1) / numeric(p2,s2)" in testDiv(BigDecimal(2)[numericOf[6, 4]], BigDecimal(2)[numericOf[4, 4]], BigDecimal(1))
  "numeric(p,s) / numeric(p,s)" in testDiv(BigDecimal(2)[numericOf[6, 4]], BigDecimal(2)[numericOf[4, 4]], BigDecimal(1))

  "float8 / float8 / float8" in {
    testQuery(
      Select(4.0[float8] / 8.0[float8] / 2.0[float8]).compile,
      "SELECT $1 / $2 / $3",
      Void
    ).head shouldBe 0.25
  }

  "float8 / (float8 / float8)" in {
    testQuery(
      Select(4.0[float8] / <<(8.0[float8] / 2.0[float8])).compile,
      "SELECT $1 / ($2 / $3)",
      Void
    ).head shouldBe 1.0
  }

  "float8 / some[float8]" in {
    testQuery(
      Select(4.0[float8] / Option(8.0)[nullable[float8]]).compile,
      "SELECT $1 / $2",
      Void
    ).head shouldBe Some(0.5)
  }

  "some[float8] / float8" in {
    testQuery(
      Select(Option(8.0)[nullable[float8]] / 4.0[float8]).compile,
      "SELECT $1 / $2",
      Void
    ).head shouldBe Some(2.0)
  }

  "some[float8] / some[float8]" in {
    testQuery(
      Select(Option(4.0)[nullable[float8]] / Option(8.0)[nullable[float8]]).compile,
      "SELECT $1 / $2",
      Void
    ).head shouldBe Some(0.5)
  }

  "float8 / null" in {
    testQuery(
      Select(4.0[float8] / Option.empty[Double][nullable[float8]]).compile,
      "SELECT $1 / $2",
      Void
    ).head shouldBe None
  }
