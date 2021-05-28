package tydal.schema

trait DbFunction[Type] extends Field[Type]:
  def dbName: String

trait DbFunction1[F, Type] extends DbFunction[Type]
trait DbFunction2[F1, F2, Type] extends DbFunction[Type]
trait DbFunction3[F1, F2, F3, Type] extends DbFunction[Type]

trait DbAggregationFunction[F, Type] extends DbFunction1[F, Type]

class Distinct[T, F <: Field[T]](field: F) extends DbFunction1[F, T]:
  def dbType: DbType[T] = field.dbType
  def dbName: String = "distinct"

class Avg[T, F <: Field[T], U](field: F)(
  using
  rational: Rational[T, U],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "avg"

class Sum[T: IsNumerical, F <: Field[T]](field: F)(
  using
  val dbType: DbType[T]
) extends DbAggregationFunction[F, T]:
  def dbName: String = "sum"

class Count[F <: Field[_]](field: F)(
  using
  val dbType: DbType[bigint]
) extends DbAggregationFunction[F, bigint]:
  def dbName: String = "count"

class Min[T, F <: Field[T], U](field: F)(
  using
  nullable: Nullable[T, U],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "min"

class Max[T, F <: Field[T], U](field: F)(
  using
  nullable: Nullable[T, U],
  val dbType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbName: String = "max"


trait Unnested[T, U]

object Unnested:
  given[T]: Unnested[array[T], T] with { }

class Unnest[T, U, F <: Field[T]](field: F)(using unnested: Unnested[T, U], val dbType: DbType[U]) extends DbFunction1[F, U]:
  def dbName: String = "unnest"
