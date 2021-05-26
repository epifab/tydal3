package tydal.schema

import scala.annotation.implicitAmbiguous

trait DbTypeProps

trait IsInteger[T] extends DbTypeProps
trait IsRational[T] extends DbTypeProps
trait IsNumeric[T] extends DbTypeProps
trait IsText[T] extends DbTypeProps
trait IsTemporal[T] extends DbTypeProps


object IsInteger:
  given IsInteger[DbType.smallint] with { }
  given IsInteger[DbType.integer] with { }
  given IsInteger[DbType.bigint] with { }
  given[T](using IsInteger[T]): IsInteger[DbType.nullable[T]] with { }


object IsRational:
  given IsRational[DbType.float4] with { }
  given IsRational[DbType.float8] with { }
  given IsRational[DbType.numeric] with { }
  given[T](using IsRational[T]): IsRational[DbType.nullable[T]] with { }


object IsNumeric:
  given integer[T](using IsInteger[T]): IsNumeric[T] with { }
  given rational[T](using IsRational[T]): IsNumeric[T] with { }


object IsText:
  given IsText[DbType.char] with { }
  given IsText[DbType.varchar] with { }
  given IsText[DbType.text] with { }
  given[T](using IsText[T]): IsText[DbType.nullable[T]] with { }


object IsTemporal:
  given IsTemporal[DbType.timestamp] with { }
  given IsTemporal[DbType.date] with { }
  given[T](using IsTemporal[T]): IsTemporal[DbType.nullable[T]] with { }
