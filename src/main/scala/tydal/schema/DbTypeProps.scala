package tydal.schema

import scala.util.NotGiven
import scala.annotation.implicitAmbiguous

trait DbTypeProps

trait IsInteger[T] extends DbTypeProps
trait IsRational[T] extends DbTypeProps
trait IsNumerical[T] extends DbTypeProps
trait IsText[T] extends DbTypeProps
trait IsTemporal[T] extends DbTypeProps


object IsInteger:
  given IsInteger[smallint] with { }
  given IsInteger[integer] with { }
  given IsInteger[bigint] with { }
  given[T](using IsInteger[T]): IsInteger[nullable[T]] with { }
  given[F, T](using FieldT[F, T], IsInteger[T]): IsInteger[F] with { }


object IsRational:
  given IsRational[float4] with { }
  given IsRational[float8] with { }
  given IsRational[numeric] with { }
  given[T](using IsRational[T]): IsRational[nullable[T]] with { }
  given[F, T](using FieldT[F, T], IsRational[T]): IsRational[F] with { }


object IsNumerical:
  given integer[T](using IsInteger[T]): IsNumerical[T] with { }
  given rational[T](using IsRational[T]): IsNumerical[T] with { }


object IsText:
  given IsText[char] with { }
  given IsText[varchar] with { }
  given IsText[text] with { }
  given[T](using IsText[T]): IsText[nullable[T]] with { }
  given[F, T](using FieldT[F, T], IsText[T]): IsText[F] with { }


object IsTemporal:
  given IsTemporal[timestamp] with { }
  given IsTemporal[date] with { }
  given[T](using IsTemporal[T]): IsTemporal[nullable[T]] with { }
  given[F, T](using FieldT[F, T], IsTemporal[T]): IsTemporal[F] with { }


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
  given field[F <: Field[_], T](using FieldT[F, T], IsNullable[T]): IsNullable[F] with { }


trait Nullable[T, U]:
  def apply(t: T): U

object Nullable:
  def apply[F, U](field: F)(using nullable: Nullable[F, U]): U = nullable(field)

  given notNullable[F <: Field[_], T](using FieldT[F, T], NotGiven[IsNullable[T]], DbType[nullable[T]]): Nullable[F, SoftCast[F, nullable[T]]] with
    def apply(f: F): SoftCast[F, nullable[T]] = SoftCast(f)

  given alreadyNullable[F <: Field[_], T](using FieldT[F, T], IsNullable[T]): Nullable[F, F] with
    def apply(f: F): F = f


trait AreComparable[T, U]

trait LowPriorityComparisons:
  given sameType[T]: AreComparable[T, T] with { }
  given leftNullable[T]: AreComparable[nullable[T], T] with { }
  given rightNullable[T]: AreComparable[T, nullable[T]] with { }
  given field[T, U, F, G](using FieldT[F, T], FieldT[G, U], AreComparable[T, U]): AreComparable[F, G] with { }

object AreComparable extends LowPriorityComparisons:
  given numeric[F, G](using IsNumerical[F], IsNumerical[G]): AreComparable[F, G] with { }
  given text[F, G](using IsText[F], IsText[G]): AreComparable[F, G] with { }
