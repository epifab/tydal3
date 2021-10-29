package tydal.test.arithmetics

import cats.kernel.Eq
import org.scalatest.Assertion
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import skunk.Void
import tydal.*
import tydal.test.IntegrationTesting

class MulSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  def testMul[A: IsNumerical, B: IsNumerical, C, Out](a: Const[A], b: Const[B], result: Out)(
    using
    ArithmeticType[A, B, C],
    DbType.Aux[C, Out],
    Eq[Out]
  ): Assertion =
    testUnique(
      Select(a * b).compile,
      "SELECT $1 * $2",
      result
    )

  "int2 * int4" in testMul(2.toShort[int2], 2[int4], 4)
  "int2 * int2" in testMul(2.toShort[int2], 2.toShort[int2], 4.toShort)
  "int2 * int8" in testMul(2.toShort[int2], 2L[int8], 4L)
  "int2 * float4" in testMul(2.toShort[int2], 2.toFloat[float4], 4.toDouble)
  "int2 * float8" in testMul(2.toShort[int2], 2.toDouble[float8], 4.toDouble)
  "int2 * numeric" in testMul(2.toShort[int2], BigDecimal(2)[numeric], BigDecimal(4))
  "int2 * numeric(p,s)" in testMul(2.toShort[int2], BigDecimal(2)[numericOf[4, 2]], BigDecimal(4))

  "int4 * int2" in testMul(2[int4], 2.toShort[int2], 4)
  "int4 * int4" in testMul(2[int4], 2[int4], 4)
  "int4 * int8" in testMul(2[int4], 2L[int8], 4L)
  "int4 * float4" in testMul(2[int4], 2.toFloat[float4], 4.0)
  "int4 * float8" in testMul(2[int4], 2.toDouble[float8], 4.0)
  "int4 * numeric" in testMul(2[int4], BigDecimal(2)[numeric], BigDecimal(4))
  "int4 * numeric(p,s)" in testMul(2[int4], BigDecimal(2)[numericOf[4, 2]], BigDecimal(4))

  "int8 * int2" in testMul(2L[int8], 2.toShort[int2], 4L)
  "int8 * int4" in testMul(2L[int8], 2[int4], 4L)
  "int8 * int8" in testMul(2L[int8], 2L[int8], 4L)
  "int8 * float4" in testMul(2L[int8], 2.toFloat[float4], 4.0)
  "int8 * float8" in testMul(2L[int8], 2.toDouble[float8], 4.0)
  "int8 * numeric" in testMul(2L[int8], BigDecimal(2)[numeric], BigDecimal(4))
  "int8 * numeric(p,s)" in testMul(2L[int8], BigDecimal(2)[numericOf[4, 2]], BigDecimal(4))

  "float4 * int2" in testMul(2.toFloat[float4], 2.toShort[int2], 4.0)
  "float4 * int4" in testMul(2.toFloat[float4], 2[int4], 4.0)
  "float4 * int8" in testMul(2.toFloat[float4], 2L[int8], 4.0)
  "float4 * float4" in testMul(2.toFloat[float4], 2.toFloat[float4], 4.toFloat)
  "float4 * float8" in testMul(2.toFloat[float4], 2.toDouble[float8], 4.0)
  "float4 * numeric" in testMul(2.toFloat[float4], BigDecimal(2)[numeric], 4.0)
  "float4 * numeric(p,s)" in testMul(2.toFloat[float4], BigDecimal(2)[numericOf[4, 2]], 4.0)

  "float8 * int2" in testMul(2.toDouble[float8], 2.toShort[int2], 4.0)
  "float8 * int4" in testMul(2.toDouble[float8], 2[int4], 4.0)
  "float8 * int8" in testMul(2.toDouble[float8], 2L[int8], 4.0)
  "float8 * float4" in testMul(2.toDouble[float8], 2.toFloat[float4], 4.0)
  "float8 * float8" in testMul(2.toDouble[float8], 2.toDouble[float8], 4.0)
  "float8 * numeric" in testMul(2.toDouble[float8], BigDecimal(2)[numeric], 4.0)
  "float8 * numeric(p,s)" in testMul(2.toDouble[float8], BigDecimal(2)[numericOf[4, 2]], 4.0)

  "numeric * int2" in testMul(BigDecimal(2)[numeric], 2.toShort[int2], BigDecimal(4))
  "numeric * int4" in testMul(BigDecimal(2)[numeric], 2[int4], BigDecimal(4.0))
  "numeric * int8" in testMul(BigDecimal(2)[numeric], 2L[int8], BigDecimal(4.0))
  "numeric * float4" in testMul(BigDecimal(2)[numeric], 2.toFloat[float4], 4.0)
  "numeric * float8" in testMul(BigDecimal(2)[numeric], 2.toDouble[float8], 4.0)
  "numeric * numeric" in testMul(BigDecimal(2)[numeric], BigDecimal(2)[numeric], BigDecimal(4))
  "numeric * numeric(p,s)" in testMul(BigDecimal(2)[numeric], BigDecimal(2)[numericOf[4, 2]], BigDecimal(4))

  "numeric(p,s) * int2" in testMul(BigDecimal(2)[numeric], 2.toShort[int2], BigDecimal(4))
  "numeric(p,s) * int4" in testMul(BigDecimal(2)[numeric], 2[int4], BigDecimal(4))
  "numeric(p,s) * int8" in testMul(BigDecimal(2)[numeric], 2L[int8], BigDecimal(4))
  "numeric(p,s) * float4" in testMul(BigDecimal(2)[numeric], 2.toFloat[float4], 4.0)
  "numeric(p,s) * float8" in testMul(BigDecimal(2)[numeric], 2.toDouble[float8], 4.0)
  "numeric(p,s) * numeric" in testMul(BigDecimal(2)[numeric], BigDecimal(2)[numericOf[4, 4]], BigDecimal(4))
  "numeric(p1,s1) * numeric(p2,s2)" in testMul(BigDecimal(2)[numericOf[6, 4]], BigDecimal(2)[numericOf[4, 4]], BigDecimal(4))
  "numeric(p,s) * numeric(p,s)" in testMul(BigDecimal(2)[numericOf[6, 4]], BigDecimal(2)[numericOf[4, 4]], BigDecimal(4))

  "(int4 * int4) * int4" in {
    testQuery(
      Select(4[int4] * 8[int4] * 12[int4]).compile,
      "SELECT $1 * $2 * $3",
      Void
    ).head shouldBe 384
  }

  "int4 * (int4 * int4)" in {
    testQuery(
      Select(4[int4] * <<(8[int4] * 12[int4])).compile,
      "SELECT $1 * ($2 * $3)",
      Void
    ).head shouldBe 384
  }

  "int4 * some[int4]" in {
    testQuery(
      Select(4[int4] * Option(8)[nullable[int4]]).compile,
      "SELECT $1 * $2",
      Void
    ).head shouldBe Some(32)
  }

  "some[int4] * int4" in {
    testQuery(
      Select(Option(8)[nullable[int4]] * 4[int4]).compile,
      "SELECT $1 * $2",
      Void
    ).head shouldBe Some(32)
  }

  "some[int4] * some[int4]" in {
    testQuery(
      Select(Option(4)[nullable[int4]] * Option(8)[nullable[int4]]).compile,
      "SELECT $1 * $2",
      Void
    ).head shouldBe Some(32)
  }

  "int4 * null" in {
    testQuery(
      Select(4[int4] * Option.empty[Int][nullable[int4]]).compile,
      "SELECT $1 * $2",
      Void
    ).head shouldBe None
  }
