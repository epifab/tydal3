package tydal.schema

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import java.util.UUID
import skunk.Codec
import skunk.codec.{all => codecs}
import skunk.data.{Type => TypeName, Arr}

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
type bool
type uuid
type date
type timestamp
type array[T]
type json[T]  // todo: encoding / decoding
type nullable[T]
type `enum`[Name, T]

trait Enumerated[T]:
  def toString(t: T): String
  def fromString(s: String): Option[T]

object DbType:
  type Aux[T, U] = DbType[T] { type Out = U }

  given DbType[varchar] with
    type Out = String
    def codec: Codec[String] = codecs.varchar
    def dbName: String = "varchar"

  given[Size <: Int](using singleton: ValueOf[Size]): DbType[varcharOf[Size]] with
    type Out = String
    def codec: Codec[String] = codecs.varchar(singleton.value)
    def dbName: String = "varchar"

  given DbType[text] with
    type Out = String
    def codec: Codec[String] = codecs.text
    def dbName: String = "text"

  given DbType[int2] with
    type Out = Short
    def codec: Codec[Short] = codecs.int2
    def dbName: String = "int2"

  given DbType[int4] with
    type Out = Int
    def codec: Codec[Int] = codecs.int4
    def dbName: String = "int4"

  given DbType[int8] with
    type Out = Long
    def codec: Codec[Long] = codecs.int8
    def dbName: String = "int8"

  given DbType[float4] with
    type Out = Float
    def codec: Codec[Float] = codecs.float4
    def dbName: String = "float4"

  given DbType[float8] with
    type Out = Double
    def codec: Codec[Double] = codecs.float8
    def dbName: String = "float8"

  given DbType[numeric] with
    type Out = BigDecimal
    def codec: Codec[BigDecimal] = codecs.numeric
    def dbName: String = "numeric"

  given DbType[bool] with
    type Out = Boolean
    def codec: Codec[Boolean] = codecs.bool
    def dbName: String = "bool"

  given DbType[uuid] with
    type Out = UUID
    def codec: Codec[UUID] = codecs.uuid
    def dbName: String = "uuid"

  given DbType[date] with
    type Out = LocalDate
    def codec: Codec[LocalDate] = codecs.date
    def dbName: String = "date"

  given DbType[timestamp] with
    type Out = Instant
    def codec: Codec[Instant] = codecs.timestamp.imap(_.atZone(ZoneOffset.UTC).toInstant)(i => LocalDateTime.ofInstant(i, ZoneOffset.UTC))
    override def dbName: String = "timestamp"

  given varcharArr: DbType[array[varchar]] with
    type Out = Arr[String]
    def codec: Codec[Arr[String]] = codecs._varchar
    override def dbName: String = "varchar[]"

  given varcharOfArr[Size <: Int](using singleton: ValueOf[Size]): DbType[array[varcharOf[Size]]] with
    type Out = Arr[String]
    def codec: Codec[Arr[String]] = codecs._varchar
    def dbName: String = "varchar[]"

  given enumArr[Name <: String, T](
    using
    singleton: ValueOf[Name],
    enumerated: Enumerated[T]
  ): DbType[array[`enum`[Name, T]]] with
    type Out = Arr[T]
    def codec: Codec[Arr[T]] = Codec.array[T](enumerated.toString, x => enumerated.fromString(x).toRight(s"Invalid element $x"), TypeName("_" + singleton.value, List(TypeName(singleton.value))))
    override def dbName: String = s"${singleton.value}[]"

  // todo: using dependent type here makes DecoderFactory fail for Scala 3.0.0
  given[T: IsNotNullable, U](using innerType: DbType.Aux[T, U]): DbType[nullable[T]] with
    type Out = Option[U]
    def codec: Codec[Option[U]] = innerType.codec.opt
    def dbName: String = innerType.dbName

  given[Name <: String, T](using singleton: ValueOf[Name], enumerated: Enumerated[T]): DbType[`enum`[Name, T]] with
    type Out = T
    def codec: Codec[T] = codecs.`enum`[T](enumerated.toString, enumerated.fromString, TypeName(singleton.value))
    def dbName: String = singleton.value
