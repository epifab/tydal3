package tydal.test

import cats.kernel.Eq
import org.scalatest.Assertion
import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.data.Arr
import skunk.{Query, Void}
import tydal.Schema._
import tydal._
import tydal.compiler._

import java.time.{Instant, LocalDate}

class DbTypesSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  given Eq[Instant] with
    override def eqv(x: Instant, y: Instant): Boolean = x == y

  given Eq[LocalDate] with
    override def eqv(x: LocalDate, y: LocalDate): Boolean = x == y

  extension[B](value: B)
    def test[A](using eq: Eq[B])(using DbType.Aux[A, B]): Assertion =
      testUnique(Select(value[A]).compile, "SELECT $1", value)

  extension[B](values: List[B])
    def testArr[A](using DbType.Aux[array[A], Arr[B]])(using Eq[Arr[B]]): Assertion =
      Arr(values: _*).test[array[A]]

  "Text" - {
    "varchar" in "hello".test[varchar]
    "varchar(size)" ignore "hello".test[varcharOf[16]]  // Postgres returns a varchar with no parameters
    "text" in "hello".test[text]
  }

  "Numbers" - {
    "int2" in 2.toShort.test[int2]
    "int4" in 2.test[int4]
    "int8" in 2L.test[int8]
    "float4" in 2.2.toFloat.test[float4]
    "float8" in 2.2.test[float8]
    "numeric" in BigDecimal(2.2).test[numeric]
    "numeric(precision,scale)" ignore BigDecimal(2.2).test[numericOf[16, 2]]  // Postgres returns a numeric with no parameters
  }

  "bool" in true.test[bool]
  "uuid" in java.util.UUID.randomUUID().test[uuid]

  "Temporal" - {
    "date" in LocalDate.now().test[date]
    "timestamp" in Instant.now().test[timestamp]
  }

  "Arrays" - {
    "array[varchar]" in List("hello", "world").testArr[varchar]
    "array[varchar(size)]" in List("hello", "world").testArr[varcharOf[16]]
    "array[text]" in List("hello", "world").testArr[text]
    "array[int2]" in List(2.toShort, 3.toShort).testArr[int2]
    "array[int4]" in List(2, 3).testArr[int4]
    "array[int8]" in List(2L, 3L).testArr[int8]
    "array[float4]" in List(2.2.toFloat, 3.3.toFloat).testArr[float4]
    "array[float8]" in List(2.2, 3.3).testArr[float8]
    "array[numeric]" in List(BigDecimal(2.2), BigDecimal(3.3)).testArr[numeric]
    "array[numeric(precision, scale)]" in List(BigDecimal(2.2), BigDecimal(3.3)).testArr[numericOf[16, 2]]
  }

  "Json" - {
    "json" in List("hello", "world").test[json[List[String]]]
    "jsonb" in List("hello", "world").test[jsonb[List[String]]]
  }

  "enum" in Genre.Rock.test[`enum`["genre", Genre]]

  "Nullable" - {
    "empty" in Option.empty[String].test[nullable[varchar]]
    "nonEmpty" in Option("hello").test[nullable[varchar]]
  }