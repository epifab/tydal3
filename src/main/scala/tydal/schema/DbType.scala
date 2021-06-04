package tydal.schema

import java.time.{LocalDate, Instant}
import java.util.UUID

trait DbType[T]:
  type Out
  def dbName: String

trait char
trait varchar
trait text
trait smallint
trait integer
trait bigint
trait float4
trait float8
trait numeric
trait bool
trait uuid
trait date
trait timestamp
trait array[T]
trait nullable[T]
trait `enum`[Name, T]

trait Enumerated[T]:
  def toString(t: T): String
  def fromString(s: String): T

object DbType:

  given DbType[char] with
    type Out = String
    def dbName: String = "char"

  given DbType[varchar] with
    type Out = String
    def dbName: String = "varchar"

  given DbType[text] with
    type Out = String
    def dbName: String = "text"

  given DbType[smallint] with
    type Out = Short
    def dbName: String = "smallint"

  given DbType[integer] with
    type Out = Int
    def dbName: String = "integer"

  given DbType[bigint] with
    type Out = Long
    def dbName: String = "bigint"

  given DbType[float4] with
    type Out = Float
    def dbName: String = "float4"

  given DbType[float8] with
    type Out = Double
    def dbName: String = "float8"

  given DbType[numeric] with
    type Out = BigDecimal
    def dbName: String = "numeric"

  given DbType[bool] with
    type Out = Boolean
    def dbName: String = "bool"

  given DbType[uuid] with
    type Out = UUID
    def dbName: String = "uuid"

  given DbType[date] with
    type Out = LocalDate
    def dbName: String = "date"

  given DbType[timestamp] with
    type Out = Instant
    override def dbName: String = "timestamp"

  given[T](using innerType: DbType[T]): DbType[array[T]] with
    type Out = Seq[innerType.Out]
    def dbName: String = s"${innerType.dbName}[]"

  given[T: IsNotNullable](using innerType: DbType[T]): DbType[nullable[T]] with
    type Out = Option[innerType.Out]
    def dbName: String = innerType.dbName

  given[Name <: String, Values](using singleton: ValueOf[Name], enumerated: Enumerated[Values]): DbType[`enum`[Name, Values]] with
    type Out = Values
    def dbName: String = singleton.value
