package tydal.schema

import scala.util.NotGiven
import scala.annotation.implicitAmbiguous

trait DbTypeProps

trait IsInteger[-T] extends DbTypeProps
trait IsRational[-T] extends DbTypeProps
trait IsNumerical[T] extends DbTypeProps
trait IsText[-T] extends DbTypeProps
trait IsTemporal[-T] extends DbTypeProps
trait IsArray[-T] extends DbTypeProps

object IsInteger:
  given IsInteger[smallint] with { }
  given IsInteger[integer] with { }
  given IsInteger[bigint] with { }
  given[T: IsInteger]: IsInteger[nullable[T]] with { }
  given[T: IsInteger]: IsInteger[Field[T]] with { }


object IsRational:
  given IsRational[float4] with { }
  given IsRational[float8] with { }
  given IsRational[numeric] with { }
  given[T: IsRational]: IsRational[nullable[T]] with { }
  given[T: IsRational]: IsRational[Field[T]] with { }


object IsNumerical:
  given integer[T: IsInteger]: IsNumerical[T] with { }
  given rational[T: IsRational]: IsNumerical[T] with { }


object IsText:
  given IsText[char] with { }
  given IsText[varchar] with { }
  given IsText[text] with { }
  given[T: IsText]: IsText[nullable[T]] with { }
  given[T: IsText]: IsText[Field[T]] with { }


object IsTemporal:
  given IsTemporal[timestamp] with { }
  given IsTemporal[date] with { }
  given[T: IsTemporal]: IsTemporal[nullable[T]] with { }
  given[T: IsTemporal]: IsTemporal[Field[T]] with { }


object IsArray:
  given[T]: IsArray[array[T]] with { }
  given[T: IsArray]: IsArray[nullable[T]] with { }
  given[T: IsArray]: IsArray[Field[T]] with { }


trait Rational[T, U]

object Rational:
  given Rational[smallint, float4] with { }
  given Rational[integer, float4] with { }
  given Rational[bigint, float8] with { }
  given nullableSmallint: Rational[nullable[smallint], nullable[float4]] with { }
  given nullableInt: Rational[nullable[integer], nullable[float4]] with { }
  given nullableBigint: Rational[nullable[bigint], nullable[float8]] with { }
  given[T: IsRational]: Rational[T, T] with { }


trait IsNullable[-T]

object IsNullable:
  given[T]: IsNullable[nullable[T]] with { }
  given[T: IsNullable]: IsNullable[Field[T]] with { }


trait Nullable[-T, U]:
  def apply(t: T): U

object Nullable:
  def apply[F, U](field: F)(using nullable: Nullable[F, U]): U = nullable(field)

  given notNullable[T: DbType, F <: Field[T]](using NotGiven[IsNullable[T]]): Nullable[F, SoftCast[F, nullable[T]]] with
    def apply(f: F): SoftCast[F, nullable[T]] = SoftCast(f)

  given alreadyNullable[T: IsNullable, F <: Field[T]]: Nullable[F, F] with
    def apply(f: F): F = f


trait AreComparable[T, U]

trait LowPriorityComparisons:
  given identity[T]: AreComparable[T, T] with { }

object AreComparable extends LowPriorityComparisons:
  given leftNullable[T, U](using AreComparable[T, U]): AreComparable[nullable[T], U] with { }
  given rightNullable[T, U](using AreComparable[T, U]): AreComparable[T, nullable[T]] with { }
  given field[T, U](using AreComparable[T, U]): AreComparable[Field[T], Field[U]] with { }
  given numeric[F: IsNumerical, G: IsNumerical]: AreComparable[F, G] with { }
  given text[F: IsText, G: IsText]: AreComparable[F, G] with { }


trait AreComparableArray[T, U]

object AreComparableArray:
  given identity[T, U] (using AreComparable[T, U]): AreComparableArray[array[T], array[U]] with { }
  given leftNullable[T, U](using AreComparable[T, U]): AreComparableArray[nullable[array[T]], array[U]] with { }
  given rightNullable[T, U](using AreComparable[T, U]): AreComparableArray[array[T], nullable[array[U]]] with { }
  given bothNullable[T, U](using AreComparable[T, U]): AreComparableArray[nullable[array[T]], nullable[array[U]]] with { }
  given field[T, U](using AreComparableArray[T, U]): AreComparableArray[Field[T], Field[U]] with { }


trait CanContain[-T, -U]

object CanContain:
  given notNullable[T, U](using AreComparable[T, U]): CanContain[array[T], U] with { }
  given leftNullable[T, U](using AreComparable[T, U]): CanContain[nullable[array[T]], U] with { }
  given rightNullable[T, U](using AreComparable[T, U]): CanContain[array[T], nullable[U]] with { }
  given bothNullable[T, U](using AreComparable[T, U]): CanContain[nullable[array[T]], nullable[U]] with { }
  given field[T, U](using CanContain[U, T]): CanContain[Field[U], Field[T]] with { }
  given subQuery[T, S, U](using SelectableT[S, U], AreComparable[T, U]): CanContain[S, Field[T]] with { }
