package tydal.schema

import scala.util.NotGiven
import scala.annotation.implicitAmbiguous

trait DbTypeProps

trait IsInteger[T] extends DbTypeProps
trait IsRational[T] extends DbTypeProps
trait IsNumeric[T] extends DbTypeProps
trait IsText[T] extends DbTypeProps
trait IsTemporal[T] extends DbTypeProps


object IsInteger:
  given IsInteger[smallint] with { }
  given IsInteger[integer] with { }
  given IsInteger[bigint] with { }
  given[T](using IsInteger[T]): IsInteger[nullable[T]] with { }


object IsRational:
  given IsRational[float4] with { }
  given IsRational[float8] with { }
  given IsRational[numeric] with { }
  given[T](using IsRational[T]): IsRational[nullable[T]] with { }


object IsNumeric:
  given integer[T](using IsInteger[T]): IsNumeric[T] with { }
  given rational[T](using IsRational[T]): IsNumeric[T] with { }


object IsText:
  given IsText[char] with { }
  given IsText[varchar] with { }
  given IsText[text] with { }
  given[T](using IsText[T]): IsText[nullable[T]] with { }


object IsTemporal:
  given IsTemporal[timestamp] with { }
  given IsTemporal[date] with { }
  given[T](using IsTemporal[T]): IsTemporal[nullable[T]] with { }


trait Rational[T, U]

object Rational:
  given Rational[smallint, float4] with { }
  given Rational[integer, float4] with { }
  given Rational[bigint, float8] with { }
  given nullableSmallint: Rational[nullable[smallint], nullable[float4]] with { }
  given nullableInt: Rational[nullable[integer], nullable[float4]] with { }
  given nullableBigint: Rational[nullable[bigint], nullable[float8]] with { }
  given[T](using IsRational[T]): Rational[T, T] with { }


trait IsNullable[T]

object IsNullable:
  given[T]: IsNullable[nullable[T]] with { }


trait Nullable[T, U]

object Nullable:
  def apply[F <: Field[_], T, U](field: F)(using fieldT: FieldT[F, T], nullable: Nullable[T, U], uType: DbType[U]): SoftCast[F, U] = SoftCast(field)
  given[T](using NotGiven[IsNullable[T]]): Nullable[T, nullable[T]] with { }
  given[T](using IsNullable[T]): Nullable[T, T] with { }
