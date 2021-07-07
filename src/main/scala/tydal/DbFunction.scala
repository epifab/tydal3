package tydal

// The result type of aggregations such as MAX and MIN can change.
// Text-like fields get, for some reason, converted to TEXT, regardless of their original type
trait MinMaxType[T, U]

trait DefaultAggregationType:
  given unchanged[T: DbType]: MinMaxType[T, T] with { }

object MinMaxType extends DefaultAggregationType:
  given textLike[T: IsText]: MinMaxType[T, nullable[text]] with { }

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


trait DbFunction[+Params <: Tuple, Type] extends Field[Type]:
  def params: Params
  def dbName: String
  def infixNotation: Boolean = false
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

extension [F <: Field[_]](field: F)
  def count: Count[F] = Count(field)


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


final class Coalesce[T, +F1 <: Field[nullable[T]], +F2 <: Field[T]](val param1: F1, val param2: F2) extends DbFunction2[F1, F2, T]:
  override val dbName: String = "COALESCE"
  override val dbType: DbType[T] = param2.dbType


trait AdditionType[T, U, V]


trait AdditionTypeNumericWithPrecisionAndScaleLowPriority:
  // Result type is the same with the standard numeric
  given numericLeft[Precision, Scale, T, U](using AdditionType[numeric, T, U]): AdditionType[numericOf[Precision, Scale], T, U] with { }
  given numericRight[Precision, Scale, T, U](using AdditionType[numeric, T, U]): AdditionType[T, numericOf[Precision, Scale], U] with { }

trait AdditionTypeNumericWithPrecisionAndScale extends AdditionTypeNumericWithPrecisionAndScaleLowPriority:
  given differentPrecisions[Precision1, Scale1, Precision2, Scale2]: AdditionType[numericOf[Precision1, Scale1], numericOf[Precision2, Scale2], numeric] with { }

trait AdditionTypeNullables:
  given[T, U, V](using AdditionType[T, U, V]): AdditionType[nullable[T], U, V] with { }
  given[T, U, V](using AdditionType[T, U, V]): AdditionType[T, nullable[U], V] with { }

object AdditionType extends AdditionTypeNullables with AdditionTypeNumericWithPrecisionAndScale:
  given[T: IsNumerical]: AdditionType[T, T, T] with { }

  given AdditionType[int2, int4, int4] with { }
  given AdditionType[int2, int8, int8] with { }
  given AdditionType[int2, float4, float8] with { }
  given AdditionType[int2, float8, float8] with { }
  given AdditionType[int2, numeric, numeric] with { }

  given AdditionType[int4, int2, int4] with { }
  given AdditionType[int4, int8, int8] with { }
  given AdditionType[int4, float4, float8] with { }
  given AdditionType[int4, float8, float8] with { }
  given AdditionType[int4, numeric, numeric] with { }

  given AdditionType[int8, int2, int8] with { }
  given AdditionType[int8, int4, int8] with { }
  given AdditionType[int8, float4, float8] with { }
  given AdditionType[int8, float8, float8] with { }
  given AdditionType[int8, numeric, numeric] with { }

  given AdditionType[float4, int2, float8] with { }
  given AdditionType[float4, int4, float8] with { }
  given AdditionType[float4, int8, float8] with { }
  given AdditionType[float4, float8, float8] with { }
  given AdditionType[float4, numeric, float8] with { }

  given AdditionType[float8, int2, float8] with { }
  given AdditionType[float8, int4, float8] with { }
  given AdditionType[float8, int8, float8] with { }
  given AdditionType[float8, float4, float8] with { }
  given AdditionType[float8, numeric, float8] with { }

  given AdditionType[numeric, int2, numeric] with { }
  given AdditionType[numeric, int4, numeric] with { }
  given AdditionType[numeric, int8, numeric] with { }
  given AdditionType[numeric, float4, float8] with { }
  given AdditionType[numeric, float8, float8] with { }


final class Add[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: AdditionType[T, U, V],
  override val dbType: DbType[V]
) extends DbFunction2[F1, F2, V]:
  override val dbName: String = "+"
  override val infixNotation: Boolean = true


final class Sub[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: AdditionType[T, U, V],
  override val dbType: DbType[V]
) extends DbFunction2[F1, F2, V]:
  override val dbName: String = "-"
  override val infixNotation: Boolean = true


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
