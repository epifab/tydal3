package tydal.schema

// The result type of aggregations such as MAX and MIN can change.
// Text-like fields get, for some reason, converted to TEXT, regardless of their original type
trait AggregationType[T, U]

trait DefaultAggregationType:
  given unchanged[T: DbType]: AggregationType[T, T] with { }

object AggregationType extends DefaultAggregationType:
  given textLike[T: IsText]: AggregationType[T, nullable[text]] with { }


trait DbFunction[+Params <: Tuple, Type] extends Field[Type]:
  def params: Params
  def dbName: String
  override def toString: String = s"$dbName$params"

trait DbFunction1[+F, Type] extends DbFunction[F *: EmptyTuple, Type]:
  def param: F
  def params: F *: EmptyTuple = param *: EmptyTuple

trait DbFunction2[+F, +G, Type] extends DbFunction[(F, G), Type]:
  def param1: F
  def param2: G
  def params: (F, G) = (param1, param2)

trait DbFunction3[+F, +G, +H, Type] extends DbFunction[(F, G, H), Type]:
  def param1: F
  def param2: G
  def param3: H
  def params: (F, G, H) = (param1, param2, param3)

trait Aggregation[+F, Type] extends DbFunction1[F, Type]

final class Avg[T, +F <: Field[T], U, +G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  rational: Rational[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "AVG"


final class Sum[T: IsNumerical, +F <: Field[T]](val param: F)(
  using
  override val dbType: DbType[T]
) extends Aggregation[F, T]:
  override val dbName: String = "SUM"

extension [T, F <: Field[T]](field: F)
  def sum(using IsNumerical[T], DbType[T]): Sum[T, F] = Sum(field)


final class Count[+F <: Field[_]](val param: F)(
  using
  val dbType: DbType[int8]
) extends Aggregation[F, int8]:
  override val dbName: String = "COUNT"

extension [F <: Field[_]](field: F)
  def count: Count[F] = Count(field)


final class Min[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  aggregationType: AggregationType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MIN"


final class Max[T, +F <: Field[T], U, G <: Field[U], V](val param: F)(
  using
  nullable: Nullable[F, G],
  aggregationType: AggregationType[U, V],
  override val dbType: DbType[V]
) extends Aggregation[F, V]:
  override val dbName: String = "MAX"


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
