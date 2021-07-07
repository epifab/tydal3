package tydal

trait Unnested[T, U]

object Unnested:
  given[T]: Unnested[array[T], T] with { }

final class Unnest[T, +F <: Field[T], U](val param: F)(
  using
  unnested: Unnested[T, U],
  override val dbType: DbType[U]
) extends DbFunction1[F, U]:
  override val dbName: String = "UNNEST"

extension [T, F <: Field[T]](field: F)
  def unnest[U](using Unnested[T, U], DbType[U]): Unnest[T, F, U] = Unnest(field)
