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

class Avg[T, F <: Field[_], U](field: F)(
  using
  ft: FieldT[F, T],
  rational: Rational[T, U],
  baseType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbType: DbType[U] = baseType
  def dbName: String = "avg"

class Sum[T, F <: Field[_]](field: F)(
  using
  ft: FieldT[F, T],
  numerical: IsNumerical[T],
  baseType: DbType[T]
) extends DbAggregationFunction[F, T]:
  def dbType: DbType[T] = baseType
  def dbName: String = "sum"

class Count[F <: Field[_]](field: F)(
  using
  baseType: DbType[bigint]
) extends DbAggregationFunction[F, bigint]:
  def dbType: DbType[bigint] = baseType
  def dbName: String = "count"

class Min[F <: Field[_], T, U](field: F)(
  using
  ft: FieldT[F, T],
  nullable: Nullable[T, U],
  baseType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbType: DbType[U] = baseType
  def dbName: String = "min"

class Max[F <: Field[_], T, U](field: F)(
  using
  ft: FieldT[F, T],
  nullable: Nullable[T, U],
  baseType: DbType[U]
) extends DbAggregationFunction[F, U]:
  def dbType: DbType[U] = baseType
  def dbName: String = "max"
