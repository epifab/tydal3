package tydal.schema

import java.time.{LocalDate, Instant}
import java.util.UUID

trait DbType[T]:
  type Type
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
    type Type = String
    def dbName: String = "char"

  given DbType[varchar] with
    type Type = String
    def dbName: String = "varchar"

  given DbType[text] with
    type Type = String
    def dbName: String = "text"

  given DbType[smallint] with
    type Type = Short
    def dbName: String = "smallint"

  given DbType[integer] with
    type Type = Int
    def dbName: String = "integer"

  given DbType[bigint] with
    type Type = Long
    def dbName: String = "bigint"

  given DbType[float4] with
    type Type = Float
    def dbName: String = "float4"

  given DbType[float8] with
    type Type = Double
    def dbName: String = "float8"

  given DbType[numeric] with
    type Type = BigDecimal
    def dbName: String = "numeric"

  given DbType[bool] with
    type Type = Boolean
    def dbName: String = "bool"

  given DbType[uuid] with
    type Type = UUID
    def dbName: String = "uuid"

  given DbType[date] with
    type Type = LocalDate
    def dbName: String = "date"

  given DbType[timestamp] with
    type Type = Instant
    override def dbName: String = "timestamp"

  given[T](using innerType: DbType[T]): DbType[array[T]] with
    type Type = Seq[innerType.Type]
    def dbName: String = s"${innerType.dbName}[]"

  given[T: IsNotNullable](using innerType: DbType[T]): DbType[nullable[T]] with
    type Type = Option[innerType.Type]
    def dbName: String = innerType.dbName

  given[Name <: String, Values](using singleton: ValueOf[Name], enumerated: Enumerated[Values]): DbType[`enum`[Name, Values]] with
    type Type = Values
    def dbName: String = singleton.value
