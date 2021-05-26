package tydal.schema

import java.time.{LocalDate, Instant}
import java.util.UUID

trait DbType[T]:
  type Type
  def dbName: String


object DbType:

  trait char
  given DbType[char] with
    type Type = String
    def dbName: String = "char"

  trait varchar
  given DbType[varchar] with
    type Type = String
    def dbName: String = "varchar"

  trait text
  given DbType[text] with
    type Type = String
    def dbName: String = "text"

  trait smallint
  given DbType[smallint] with
    type Type = Short
    def dbName: String = "smallint"

  trait integer
  given DbType[integer] with
    type Type = Int
    def dbName: String = "integer"

  trait bigint
  given DbType[bigint] with
    type Type = Long
    def dbName: String = "bigint"

  trait numeric
  given DbType[numeric] with
    type Type = BigDecimal
    def dbName: String = "numeric"

  trait float4
  given DbType[float4] with
    type Type = Float
    def dbName: String = "float4"

  trait float8
  given DbType[float8] with
    type Type = Double
    def dbName: String = "float8"

  trait bool
  given DbType[bool] with
    type Type = Boolean
    def dbName: String = "bool"

  trait uuid
  given DbType[uuid] with
    type Type = UUID
    def dbName: String = "uuid"

  trait date
  given DbType[date] with
    type Type = LocalDate
    def dbName: String = "date"

  trait timestamp
  given DbType[timestamp] with
    type Type = Instant
    override def dbName: String = "timestamp"

  trait array[T]
  given[T](using innerType: DbType[T]): DbType[array[T]] with
    type Type = Seq[innerType.Type]
    def dbName: String = s"${innerType.dbName}[]"

  trait nullable[T]
  given[T](using innerType: DbType[T]): DbType[nullable[T]] with
    type Type = Option[innerType.Type]
    def dbName: String = innerType.dbName

  trait `enum`[Name, T]

  trait Enumerated[T]:
    def toString(t: T): String
    def fromString(s: String): T

  given[Name <: String, Values](using singleton: ValueOf[Name], enumerated: Enumerated[Values]): DbType[`enum`[Name, Values]] with
    type Type = Values
    def dbName: String = singleton.value
