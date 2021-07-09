package tydal

import io.circe.{Decoder => JsonDecoder, Encoder => JsonEncoder}
import skunk.Codec
import skunk.circe.codec.{all => jsonCodecs}
import skunk.codec.{all => codecs}
import skunk.data.{Arr, Type}

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID

trait DbType[T]:
  type Out
  def codec: Codec[Out]
  def dbName: String

type varchar
type varcharOf[size]
type text
type int2
type int4
type int8
type float4
type float8
type numeric
type numericOf[precision, scale]
type bool
type uuid
type date
type timestamp
type array[T]
type json[T]
type jsonb[T]
type nullable[T]
type `enum`[Name, T]

trait Enumerated[T]:
  def toString(t: T): String
  def fromString(s: String): Option[T]

object DbType:
  type Aux[T, U] = DbType[T] { type Out = U }

  given DbType[varchar] with
    type Out = String
    override val codec: Codec[String] = codecs.varchar
    override val dbName: String = "varchar"

  given[Size <: Int](using size: ValueOf[Size]): DbType[varcharOf[Size]] with
    type Out = String
    override val codec: Codec[String] = codecs.varchar(size.value)
    override val dbName: String = "varchar"

  given DbType[text] with
    type Out = String
    override val codec: Codec[String] = codecs.text
    override val dbName: String = "text"

  given DbType[int2] with
    type Out = Short
    override val codec: Codec[Short] = codecs.int2
    override val dbName: String = "int2"

  given DbType[int4] with
    type Out = Int
    override val codec: Codec[Int] = codecs.int4
    override val dbName: String = "int4"

  given DbType[int8] with
    type Out = Long
    override val codec: Codec[Long] = codecs.int8
    override val dbName: String = "int8"

  given DbType[float4] with
    type Out = Float
    override val codec: Codec[Float] = codecs.float4
    override val dbName: String = "float4"

  given DbType[float8] with
    type Out = Double
    override val codec: Codec[Double] = codecs.float8
    override val dbName: String = "float8"

  given DbType[numeric] with
    type Out = BigDecimal
    override val codec: Codec[BigDecimal] = codecs.numeric
    override val dbName: String = "numeric"

  given [Precision <: Int, Scale <: Int](using precision: ValueOf[Precision], scale: ValueOf[Scale]): DbType[numericOf[Precision, Scale]] with
    type Out = BigDecimal
    override val codec: Codec[BigDecimal] = codecs.numeric(precision.value, scale.value)
    override val dbName: String = "numeric"

  given DbType[bool] with
    type Out = Boolean
    override val codec: Codec[Boolean] = codecs.bool
    override val dbName: String = "bool"

  given DbType[uuid] with
    type Out = UUID
    override val codec: Codec[UUID] = codecs.uuid
    override val dbName: String = "uuid"

  given DbType[date] with
    type Out = LocalDate
    override val codec: Codec[LocalDate] = codecs.date
    override val dbName: String = "date"

  given DbType[timestamp] with
    type Out = Instant
    override val codec: Codec[Instant] = codecs.timestamp.imap(_.atZone(ZoneOffset.UTC).toInstant)(i => LocalDateTime.ofInstant(i, ZoneOffset.UTC))
    override val dbName: String = "timestamp"

  given int2Arr: DbType[array[int2]] with
    type Out = Arr[Short]
    override val codec: Codec[Arr[Short]] = codecs._int2
    override val dbName: String = "int2[]"

  given int4Arr: DbType[array[int4]] with
    type Out = Arr[Int]
    override val codec: Codec[Arr[Int]] = codecs._int4
    override val dbName: String = "int4[]"

  given int8Arr: DbType[array[int8]] with
    type Out = Arr[Long]
    override val codec: Codec[Arr[Long]] = codecs._int8
    override val dbName: String = "int8[]"

  given float4Arr: DbType[array[float4]] with
    type Out = Arr[Float]
    override val codec: Codec[Arr[Float]] = codecs._float4
    override val dbName: String = "float4[]"

  given float8Arr: DbType[array[float8]] with
    type Out = Arr[Double]
    override val codec: Codec[Arr[Double]] = codecs._float8
    override val dbName: String = "float8[]"

  given numericArr: DbType[array[numeric]] with
    type Out = Arr[BigDecimal]
    override val codec: Codec[Arr[BigDecimal]] = codecs._numeric
    override val dbName: String = "numeric[]"

  given numericOfArr[P <: Int, S <: Int](using precision: ValueOf[P], scale: ValueOf[S]): DbType[array[numericOf[P, S]]] with
    type Out = Arr[BigDecimal]
    override val codec: Codec[Arr[BigDecimal]] = Codec.array[BigDecimal](_.toString, s => scala.util.Try(BigDecimal(s)).toEither.left.map(_ => "NaN"), Type(s"_numeric(${precision.value},${scale.value})", List(Type.numeric(precision.value, scale.value))))
    override val dbName: String = s"numeric(${precision.value},${scale.value})[]"

  given uuidArr: DbType[array[uuid]] with
    type Out = Arr[UUID]
    override val codec: Codec[Arr[UUID]] = Codec.array[UUID](_.toString, s => scala.util.Try(UUID.fromString(s)).toEither.left.map(_ => "Invalid UUID"), Type._uuid)
    override val dbName: String = "uuid[]"

  given varcharArr: DbType[array[varchar]] with
    type Out = Arr[String]
    override val codec: Codec[Arr[String]] = codecs._varchar
    override val dbName: String = "varchar[]"

  given varcharOfArr[Size <: Int](using singleton: ValueOf[Size]): DbType[array[varcharOf[Size]]] with
    type Out = Arr[String]
    override val codec: Codec[Arr[String]] = codecs._varchar
    override val dbName: String = "varchar[]"

  given textArr: DbType[array[text]] with
    type Out = Arr[String]
    override val codec: Codec[Arr[String]] = codecs._text
    override val dbName: String = "text[]"

  given enumArr[Name <: String, T](
    using
    singleton: ValueOf[Name],
    enumerated: Enumerated[T]
  ): DbType[array[`enum`[Name, T]]] with
    type Out = Arr[T]
    override val codec: Codec[Arr[T]] = Codec.array[T](enumerated.toString, x => enumerated.fromString(x).toRight(s"Invalid element $x"), Type("_" + singleton.value, List(Type(singleton.value))))
    override val dbName: String = s"${singleton.value}[]"

  given[T](using enc: JsonEncoder[T], dec: JsonDecoder[T]): DbType[json[T]] with
    type Out = T
    override val codec: Codec[T] = jsonCodecs.json
    override val dbName: String = "json"

  given[T](using enc: JsonEncoder[T], dec: JsonDecoder[T]): DbType[jsonb[T]] with
    type Out = T
    override val codec: Codec[T] = jsonCodecs.jsonb
    override val dbName: String = "json"

  // todo: using dependent type here makes DecoderFactory fail for Scala 3.0.0
  given[T: IsNotNullable, U](using innerType: DbType.Aux[T, U]): DbType[nullable[T]] with
    type Out = Option[U]
    override val codec: Codec[Option[U]] = innerType.codec.opt
    override val dbName: String = innerType.dbName

  given[Name <: String, T](using singleton: ValueOf[Name], enumerated: Enumerated[T]): DbType[`enum`[Name, T]] with
    type Out = T
    override val codec: Codec[T] = codecs.`enum`[T](enumerated.toString, enumerated.fromString, Type(singleton.value))
    override val dbName: String = singleton.value
