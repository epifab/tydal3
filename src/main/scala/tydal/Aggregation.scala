package tydal

trait Aggregation[+F, Type] extends DbFunction1[F, Type]:
  def distinct: Boolean

final class Avg[T, +F <: Field[T], U, +G <: Field[U], V] private(val param: F, override val distinct: Boolean)(
  using
  nullable: Nullable[F, G],
  rational: Rational[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "AVG"

object Avg:
  def apply[T, F <: Field[T], U, G <: Field[U], V](param: F)(
    using
    Nullable[F, G],
    Rational[U, V],
    DbType[V]
  ): Avg[T, F, U, G, V] = new Avg(param, false)

  def distinct[T, F <: Field[T], U, G <: Field[U], V](param: F)(
    using
    Nullable[F, G],
    Rational[U, V],
    DbType[V]
  ): Avg[T, F, U, G, V] = new Avg(param, true)


// The result type of SUM
trait SumType[T, U]

object SumType:
  given SumType[int2, int8] with { }
  given SumType[int4, int8] with { }
  given SumType[int8, numeric] with { }
  given SumType[float4, float4] with { }
  given SumType[float8, float8] with { }
  given SumType[numeric, numeric] with { }
  given [T, U](using SumType[T, U]): SumType[nullable[T], nullable[U]] with { }

final class Sum[T: IsNumerical, +F <: Field[T], U, +G <: Field[U], V] private(val param: F, override val distinct: Boolean)(
  using
  nullable: Nullable[F, G],
  sumType: SumType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "SUM"

object Sum:
  def apply[T: IsNumerical, F <: Field[T], U, G <: Field[U], V](param: F)(
    using
    // The result of SUM is not necessarily nullable, but it's just too complicate to distinguish the cases
    Nullable[F, G],
    SumType[U, V],
    DbType[V]
  ): Sum[T, F, U, G, V] = new Sum(param, false)

  def distinct[T: IsNumerical, F <: Field[T], U, G <: Field[U], V](param: F)(
    using
    Nullable[F, G],
    SumType[U, V],
    DbType[V]
  ): Sum[T, F, U, G, V] = new Sum(param, true)


final class Count[+F <: Field[_]] private(val param: F, override val distinct: Boolean)(
  using
  val dbType: DbType[int8]
) extends Aggregation[F, int8]:
  override val dbName: String = "COUNT"


object Count:
  def apply[F <: Field[_]](param: F): Count[F] = new Count(param, false)
  def distinct[F <: Field[_]](param: F): Count[F] = new Count(param, true)


// The result type of aggregations such as MAX and MIN can change.
// Text-like fields get, for some reason, converted to TEXT, regardless of their original type
trait MinMaxType[T, U]

trait DefaultMinMaxType:
  given unchanged[T: DbType]: MinMaxType[T, T] with { }

object MinMaxType extends DefaultMinMaxType:
  given textLike[T: IsText]: MinMaxType[T, nullable[text]] with { }


final class Min[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  // The result of MIN is not necessarily nullable, but it's just too complicate to distinguish the cases
  nullable: Nullable[F, G],
  aggregationType: MinMaxType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MIN"
  override val distinct: Boolean = false  // MIN(DISTINCT) is truly meaningless


final class Max[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  // The result of MAX is not necessarily nullable, but it's just too complicate to distinguish the cases
  nullable: Nullable[F, G],
  aggregationType: MinMaxType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MAX"
  override val distinct: Boolean = false  // MAX(DISTINCT) is truly meaningless
