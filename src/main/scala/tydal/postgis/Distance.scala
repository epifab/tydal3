package tydal
package postgis

import tydal.utils.AnyNullableAlgebra

class Distance[F <: Field[_]: IsGeography, G <: Field[_]: IsGeography, U](val param1: F, val param2: G)(using alg: AnyNullableAlgebra[(F, G), float8, U]) extends DbFunction2[F, G, U]:
  override val dbType: DbType[U] = alg.get
  override val dbName: String = "ST_Distance"
