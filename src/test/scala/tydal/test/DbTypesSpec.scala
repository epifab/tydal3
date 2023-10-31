package tydal.test

import cats.kernel.Eq
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import org.scalatest.Assertion
import skunk.{Query, Void}
import skunk.data.Arr
import tydal.*
import tydal.compiler.*
import tydal.Schema.*

import java.time.{Instant, LocalDate}
import java.time.temporal.ChronoUnit
import java.util.UUID

object all_supported_types
    extends TableSchema[
      "all_supported_types",
      (
          "varchar" :=: varchar,
          "varcharOf[16]" :=: varcharOf[16],
          "int2" :=: int2,
          "int4" :=: int4,
          "int8" :=: int8,
          "float4" :=: float4,
          "float8" :=: float8,
          "numeric" :=: numeric,
          "numericOf[10,2]" :=: numericOf[10, 2],
          "bool" :=: bool,
          "uuid" :=: uuid,
          "date" :=: date,
          "timestamp" :=: timestamp,
          "array[varchar]" :=: array[varchar],
          "array[varcharOf[16]]" :=: array[varcharOf[16]],
          "array[text]" :=: array[text],
          "array[int2]" :=: array[int2],
          "array[int4]" :=: array[int4],
          "array[int8]" :=: array[int8],
          "array[float4]" :=: array[float4],
          "array[float8]" :=: array[float8],
          "array[numeric]" :=: array[numeric],
          "array[numericOf[10,2]]" :=: array[numericOf[10, 2]],
          "array[uuid]" :=: array[uuid],
          "enum" :=: `enum`["genre", Genre],
          "json" :=: json[String],
          "jsonb" :=: jsonb[String],
          "geography" :=: postgis.geography,
          "geometry" :=: postgis.geometry,
          "point" :=: postgis.point
      )
    ]

class DbTypesSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  val insertAllSupportedTypes =
    session.flatMap(_.prepareR(Insert.into(all_supported_types).compile))

  val selectAllSupportedTypes =
    session.flatMap(
      _.prepareR(
        Select
          .from(all_supported_types as "a")
          .take(_("a").*)
          .where(_("a", "uuid") === "uuid?")
          .compile
      )
    )

  extension [B](value: B)
    def test[A](using eq: Eq[B])(using DbType.Aux[A, B]): Assertion =
      testUnique(Select(value[A]).compile, "SELECT $1", value)

  extension [B](values: List[B])
    def testArr[A](using DbType.Aux[array[A], Arr[B]])(using Eq[Arr[B]]): Assertion =
      Arr(values*).test[array[A]]

  "Text" - {
    "varchar" in "hello".test[varchar]
    "varchar(size)" ignore "hello"
      .test[varcharOf[16]] // Postgres returns a varchar with no parameters
    "text" in "hello".test[text]
  }

  "Numbers" - {
    "int2" in 2.toShort.test[int2]
    "int4" in 2.test[int4]
    "int8" in 2L.test[int8]
    "float4" in 2.2.toFloat.test[float4]
    "float8" in 2.2.test[float8]
    "numeric" in BigDecimal(2.2).test[numeric]
    "numeric(precision,scale)" ignore BigDecimal(2.2)
      .test[numericOf[16, 2]] // Postgres returns a numeric with no parameters
  }

  "bool" in true.test[bool]
  "uuid" in java.util.UUID.randomUUID().test[uuid]

  "Temporal" - {
    "date" in LocalDate.now.test[date]
    "timestamp" in Instant.now().truncatedTo(ChronoUnit.MICROS).test[timestamp]
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
    // todo: the following fails decoding _numeric
    "array[numeric(precision, scale)]" ignore List(BigDecimal(2.2), BigDecimal(3.3))
      .testArr[numericOf[16, 2]]
  }

  "Json" - {
    "json" in List("hello", "world").test[json[List[String]]]
    "jsonb" in List("hello", "world").test[jsonb[List[String]]]
  }

  "Enum" in Genre.Rock.test[`enum`["genre", Genre]]

  "Nullable" - {
    "empty" in Option.empty[String].test[nullable[varchar]]
    "nonEmpty" in Option("hello").test[nullable[varchar]]
  }

  "Postgis" - {
    "geography" in "0101000020E6100000FA731B1E0C364A40F4C70FF0FF461340".test[postgis.geography]
    "geometry" in "0101000020E6100000FA731B1E0C364A40F4C70FF0FF461340".test[postgis.geometry]
    "point" in (52.4222448, 4.8193357).test[postgis.point]
  }

  "All supported types" in {
    val uuid             = UUID.randomUUID()
    val date: LocalDate  = LocalDate.now()
    val instant: Instant = Instant.now().truncatedTo(ChronoUnit.MICROS)

    val actual = (for
      _ <- insertAllSupportedTypes.use(
        _.execute(
          "varchar" ~~> "hello",
          "varcharOf[16]" ~~> "hello",
          "int2" ~~> 2.toShort,
          "int4" ~~> 2,
          "int8" ~~> 2L,
          "float4" ~~> 2.toFloat,
          "float8" ~~> 2.toDouble,
          "numeric" ~~> BigDecimal(2),
          "numericOf[10,2]" ~~> BigDecimal(2),
          "bool" ~~> true,
          "uuid" ~~> uuid,
          "date" ~~> date,
          "timestamp" ~~> instant,
          "array[varchar]" ~~> Arr("hello"),
          "array[varcharOf[16]]" ~~> Arr("hello"),
          "array[text]" ~~> Arr("hello", "hello"),
          "array[int2]" ~~> Arr(2.toShort, 2.toShort),
          "array[int4]" ~~> Arr(2, 2),
          "array[int8]" ~~> Arr(2.toLong, 2.toLong),
          "array[float4]" ~~> Arr(2.toFloat, 2.toFloat),
          "array[float8]" ~~> Arr(2.toDouble, 2.toDouble),
          "array[numeric]" ~~> Arr(BigDecimal(2), BigDecimal(2)),
          "array[numericOf[10,2]]" ~~> Arr(BigDecimal(2), BigDecimal(2)),
          "array[uuid]" ~~> Arr(uuid, uuid),
          "enum" ~~> Genre.Rock,
          "json" ~~> "hello",
          "jsonb" ~~> "hello",
          "geography" ~~> "0101000020E6100000FA731B1E0C364A40F4C70FF0FF461340",
          "geometry" ~~> "0101000020E6100000FA731B1E0C364A40F4C70FF0FF461340",
          "point" ~~> (52.4222448, 4.8193357)
        )
      )
      result <- selectAllSupportedTypes.use(_.unique("uuid?" ~~> uuid))
    yield result).unsafeRunSync()

    assertEq(actual(0), "hello")
    assertEq(actual(1), "hello")
    assertEq(actual(2), 2.toShort)
    assertEq(actual(3), 2)
    assertEq(actual(4), 2L)
    assertEq(actual(5), 2.toFloat)
    assertEq(actual(6), 2.toDouble)
    assertEq(actual(7), BigDecimal(2))
    assertEq(actual(8), BigDecimal(2.00))
    assertEq(actual(9), true)
    assertEq(actual(10), uuid)
    assertEq(actual(11), date)
    assertEq(actual(12), instant)
    assertEq(actual(13), Arr("hello"))
    assertEq(actual(14), Arr("hello"))
    assertEq(actual(15), Arr("hello", "hello"))
    assertEq(actual(16), Arr(2.toShort, 2.toShort))
    assertEq(actual(17), Arr(2, 2))
    assertEq(actual(18), Arr(2.toLong, 2.toLong))
    assertEq(actual(19), Arr(2.toFloat, 2.toFloat))
    assertEq(actual(20), Arr(2.toDouble, 2.toDouble))
    assertEq(actual(21), Arr(BigDecimal(2), BigDecimal(2)))
    assertEq(actual(22), Arr(BigDecimal(2.00), BigDecimal(2.00)))
    assertEq(actual(23), Arr(uuid, uuid))
    assertEq(actual(24), Genre.Rock)
    assertEq(actual(25), "hello")
    assertEq(actual(26), "hello")
  }
