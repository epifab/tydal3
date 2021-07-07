package tydal

final class Coalesce[T, +F1 <: Field[nullable[T]], +F2 <: Field[T]](val param1: F1, val param2: F2) extends DbFunction2[F1, F2, T]:
  override val dbName: String = "COALESCE"
  override val dbType: DbType[T] = param2.dbType
