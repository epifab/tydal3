package tydal
package postgis

import tydal.utils.AnyNullableAlgebra

class Latitude[F <: Field[_]: IsGeometry, U](val param: F)(using alg: AnyNullableAlgebra[F, float8, U]) extends DbFunction1[F, U]:
  override val dbName: String = "ST_X"
  override val dbType: DbType[U] = alg.get
