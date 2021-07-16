package tydal
package postgis

import tydal.utils.AnyNullableAlgebra

class Longitude[F <: Field[_]: IsPoint, U](val param: F)(using alg: AnyNullableAlgebra[F, float8, U]) extends DbFunction1[F, U]:
  override val dbName: String = "ST_Y"
  override val dbType: DbType[U] = alg.get
