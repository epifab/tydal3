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


trait AddSubType[T, U, V]

// todo: I was assuming that Postgres would apply a smart cast (e.g. float4 + int8 = float8, but it doesn't
//  what it does instead is pretty unexplaninable to me, so an explicit cast is possibly preferable
//
//trait AddSubInt4:
//  given rationalLeft[T: IsRational]: AddSubType[T, int4, float4] with { }
//  given rationalRight[T: IsRational]: AddSubType[int4, T, float4] with { }
//  given integerLeft[T: IsInteger]: AddSubType[T, int4, int4] with { }
//  given integerRight[T: IsInteger]: AddSubType[int4, T, int4] with { }
//
//trait AddSubFloat4 extends AddSubInt4:
//  given float4Right[T: IsNumerical]: AddSubType[T, float4, float4] with { }
//  given float4Left[T: IsNumerical]: AddSubType[float4, T, float4] with { }
//
//trait AddSubInt8 extends AddSubFloat4:
//  given rational8Left[T: IsRational]: AddSubType[T, int8, float8] with { }
//  given rational8Right[T: IsRational]: AddSubType[int8, T, float8] with { }
//  given integer8Left[T: IsInteger]: AddSubType[T, int8, int8] with { }
//  given integer8Right[T: IsInteger]: AddSubType[int8, T, int8] with { }
//
//trait AddSubFloat8 extends AddSubInt8:
//  given float8Right[T: IsNumerical]: AddSubType[T, float8, float8] with { }
//  given float8Left[T: IsNumerical]: AddSubType[float8, T, float8] with { }
//
//trait AddSubNumerical extends AddSubFloat8:
//  given numericRight[T: IsNumerical]: AddSubType[T, numeric, numeric] with { }
//  given numericLeft[T: IsNumerical]: AddSubType[numeric, T, numeric] with { }

trait AddSubTypeNullables:
  given[T, U, V](using AddSubType[T, U, V]): AddSubType[nullable[T], U, V] with { }
  given[T, U, V](using AddSubType[T, U, V]): AddSubType[T, nullable[U], V] with { }

object AddSubType extends AddSubTypeNullables:
  given[T: IsNumerical]: AddSubType[T, T, T] with { }


final class Add[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  addSubType: AddSubType[T, U, V],
  override val dbType: DbType[V]
) extends DbFunction2[F1, F2, V]:
  override val dbName: String = "+"
  override val infixNotation: Boolean = true


final class Sub[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  addSubType: AddSubType[T, U, V],
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
