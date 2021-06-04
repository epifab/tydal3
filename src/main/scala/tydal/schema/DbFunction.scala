package tydal.schema

trait DbFunction[Params <: Tuple, Type] extends Field[Type]:
  def params: Params
  def dbName: String
  override def toString: String = s"$dbName$params"

trait DbFunction1[F, Type] extends DbFunction[F *: EmptyTuple, Type]:
  def param: F
  def params: F *: EmptyTuple = param *: EmptyTuple

trait DbFunction2[F, G, Type] extends DbFunction[(F, G), Type]:
  def param1: F
  def param2: G
  def params: (F, G) = (param1, param2)

trait DbFunction3[F, G, H, Type] extends DbFunction[(F, G, H), Type]:
  def param1: F
  def param2: G
  def param3: H
  def params: (F, G, H) = (param1, param2, param3)

trait DbAggregationFunction[F, Type] extends DbFunction1[F, Type]

final class Distinct[T, F <: Field[T]](val param: F) extends DbFunction1[F, T]:
  def dbType: DbType[T] = param.dbType
  def dbName: String = "distinct"

final class Avg[T, F <: Field[T], U](val param: F)(
  using
  rational: Rational[T, U],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "avg"

final class Sum[T: IsNumerical, F <: Field[T]](val param: F)(
  using
  val dbType: DbType[T]
) extends DbAggregationFunction[F, T]:
  def dbName: String = "sum"

final class Count[F <: Field[_]](val param: F)(
  using
  val dbType: DbType[bigint]
) extends DbAggregationFunction[F, bigint]:
  def dbName: String = "count"

final class Min[T, F <: Field[T], U, G <: Field[U]](val param: F)(
  using
  nullable: Nullable[F, G],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "min"

final class Max[T, F <: Field[T], U, G <: Field[U]](val param: F)(
  using
  nullable: Nullable[F, G],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "max"


trait Unnested[T, U]

object Unnested:
  given[T]: Unnested[array[T], T] with { }

final class Unnest[T, U, F <: Field[T]](val param: F)(using unnested: Unnested[T, U], val dbType: DbType[U]) extends DbFunction1[F, U]:
  def dbName: String = "unnest"
