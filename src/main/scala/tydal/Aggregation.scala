package tydal

trait Aggregation[+F, Type] extends DbFunction1[F, Type]

final class Avg[T, +F <: Field[T], U, +G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  rational: Rational[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "AVG"


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

final class Sum[T: IsNumerical, +F <: Field[T], U, +G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  sumType: SumType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "SUM"


final class Count[+F <: Field[_]](val param: F)(
  using
  val dbType: DbType[int8]
) extends Aggregation[F, int8]:
  override val dbName: String = "COUNT"


// The result type of aggregations such as MAX and MIN can change.
// Text-like fields get, for some reason, converted to TEXT, regardless of their original type
trait MinMaxType[T, U]

trait DefaultMinMaxType:
  given unchanged[T: DbType]: MinMaxType[T, T] with { }

object MinMaxType extends DefaultMinMaxType:
  given textLike[T: IsText]: MinMaxType[T, nullable[text]] with { }


final class Min[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  aggregationType: MinMaxType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MIN"


final class Max[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  aggregationType: MinMaxType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MAX"
