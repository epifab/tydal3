package tydal

trait Arithemtic

final class Add[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: ArithmeticType[T, U, V],
  override val dbType: DbType[V]
) extends Arithemtic with DbFunction2[F1, F2, V]:
  override val dbName: String = "+"
  override val infixNotation: Boolean = true

final class Sub[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: ArithmeticType[T, U, V],
  override val dbType: DbType[V]
) extends Arithemtic with DbFunction2[F1, F2, V]:
  override val dbName: String = "-"
  override val infixNotation: Boolean = true

final class Mul[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: ArithmeticType[T, U, V],
  override val dbType: DbType[V]
) extends Arithemtic with DbFunction2[F1, F2, V]:
  override val dbName: String = "*"
  override val infixNotation: Boolean = true

final class Div[T: IsNumerical, +F1 <: Field[T], U: IsNumerical, +F2 <: Field[U], V](val param1: F1, val param2: F2)(
  using
  additionType: ArithmeticType[T, U, V],
  override val dbType: DbType[V]
) extends Arithemtic with DbFunction2[F1, F2, V]:
  override val dbName: String = "/"
  override val infixNotation: Boolean = true


trait ArithmeticType[T, U, V]

trait AdditionTypeNumericWithPrecisionAndScaleLowPriority:
  // Result type is the same with the standard numeric
  given numericLeft[Precision, Scale, T, U](using ArithmeticType[numeric, T, U]): ArithmeticType[numericOf[Precision, Scale], T, U] with { }
  given numericRight[Precision, Scale, T, U](using ArithmeticType[numeric, T, U]): ArithmeticType[T, numericOf[Precision, Scale], U] with { }

trait AdditionTypeNumericWithPrecisionAndScale extends AdditionTypeNumericWithPrecisionAndScaleLowPriority:
  given differentPrecisions[Precision1, Scale1, Precision2, Scale2]: ArithmeticType[numericOf[Precision1, Scale1], numericOf[Precision2, Scale2], numeric] with { }

trait AdditionTypeNullables:
  given[T, U, V](using ArithmeticType[T, U, V]): ArithmeticType[nullable[T], U, nullable[V]] with { }
  given[T, U, V](using ArithmeticType[T, U, V]): ArithmeticType[T, nullable[U], nullable[V]] with { }

object ArithmeticType extends AdditionTypeNullables with AdditionTypeNumericWithPrecisionAndScale:
  given[T: IsNumerical]: ArithmeticType[T, T, T] with { }

  given ArithmeticType[int2, int4, int4] with { }
  given ArithmeticType[int2, int8, int8] with { }
  given ArithmeticType[int2, float4, float8] with { }
  given ArithmeticType[int2, float8, float8] with { }
  given ArithmeticType[int2, numeric, numeric] with { }

  given ArithmeticType[int4, int2, int4] with { }
  given ArithmeticType[int4, int8, int8] with { }
  given ArithmeticType[int4, float4, float8] with { }
  given ArithmeticType[int4, float8, float8] with { }
  given ArithmeticType[int4, numeric, numeric] with { }

  given ArithmeticType[int8, int2, int8] with { }
  given ArithmeticType[int8, int4, int8] with { }
  given ArithmeticType[int8, float4, float8] with { }
  given ArithmeticType[int8, float8, float8] with { }
  given ArithmeticType[int8, numeric, numeric] with { }

  given ArithmeticType[float4, int2, float8] with { }
  given ArithmeticType[float4, int4, float8] with { }
  given ArithmeticType[float4, int8, float8] with { }
  given ArithmeticType[float4, float8, float8] with { }
  given ArithmeticType[float4, numeric, float8] with { }

  given ArithmeticType[float8, int2, float8] with { }
  given ArithmeticType[float8, int4, float8] with { }
  given ArithmeticType[float8, int8, float8] with { }
  given ArithmeticType[float8, float4, float8] with { }
  given ArithmeticType[float8, numeric, float8] with { }

  given ArithmeticType[numeric, int2, numeric] with { }
  given ArithmeticType[numeric, int4, numeric] with { }
  given ArithmeticType[numeric, int8, numeric] with { }
  given ArithmeticType[numeric, float4, float8] with { }
  given ArithmeticType[numeric, float8, float8] with { }

