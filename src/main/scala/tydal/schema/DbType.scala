package tydal.schema

import java.time.{LocalDate, Instant}
import java.util.UUID

trait DbType[T]:
  type Type
  def dbName: String


object DbType:

  trait varchar
  given DbType[varchar] with
    type Type = String
    def dbName: String = "varchar"

  trait text
  given DbType[text] with
    type Type = String
    def dbName: String = "text"

  trait int
  given DbType[int] with
    type Type = Int
    def dbName: String = "int"

  trait bool
  given DbType[bool] with
    type Type = Boolean
    def dbName: String = "bool"

  trait uuid
  given DbType[uuid] with
    type Type = UUID
    def dbName: String = "uuid"

  trait float
  given DbType[float] with
    type Type = Double
    def dbName: String = "float"

  trait date
  given DbType[date] with
    type Type = LocalDate
    def dbName: String = "date"

  trait timestamp
  given DbType[timestamp] with
    type Type = Instant
    override def dbName: String = "timestamp"

  trait bigInt
  given DbType[bigInt] with
    type Type = Long
    def dbName: String = "bigint"

  trait array[T]
  given[T](using innerType: DbType[T]): DbType[array[T]] with
    type Type = Seq[innerType.Type]
    def dbName: String = s"${innerType.dbName}[]"

  trait nullable[T]
  given[T](using innerType: DbType[T]): DbType[nullable[T]] with
    type Type = Option[innerType.Type]
    def dbName: String = innerType.dbName

  trait enumeration[Name, T]

  trait Enumerated[T]:
    def toString(t: T): String
    def fromString(s: String): T

  given[Name <: String, Values](using singleton: ValueOf[Name], enumerated: Enumerated[Values]): DbType[enumeration[Name, Values]] with
    type Type = Values
    def dbName: String = singleton.value
