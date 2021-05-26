package tydal.schema

trait DbFunction[Type] extends Field[Type]

trait DbFunction1[F, Type] extends DbFunction[Type]
trait DbFunction2[F1, F2, Type] extends DbFunction[Type]
trait DbAggregationFunction[F, Type] extends DbFunction1[F, Type]

class Distinct[T, F <: Field[T]](field: F) extends DbFunction1[F, T]:
  override def dbType: DbType[T] = field.dbType
