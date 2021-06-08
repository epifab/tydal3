package tydal.schema

sealed trait LogicalExpr:
  def and[F2 <: LogicalExpr](otherExpression: F2): And[this.type, F2] = And(this, otherExpression)
  def or[F2 <: LogicalExpr](otherExpression: F2): Or[this.type, F2] = Or(this, otherExpression)

sealed trait AlwaysTrue extends LogicalExpr

case object AlwaysTrue extends AlwaysTrue

sealed trait LogicalExpr1[F] extends LogicalExpr:
  def expr: F

sealed trait LogicalExpr2[F1, F2](sqlRepr: String) extends LogicalExpr:
  def left: F1
  def right: F2
  override def toString: String = s"$left $sqlRepr $right"

case class IsDefined[F <: Field[_]](expr: F)(using IsNullable[F])
  extends LogicalExpr1[F]

case class IsNotDefined[F <: Field[_]](expr: F)(using IsNullable[F])
  extends LogicalExpr1[F]

case class And[F1 <: LogicalExpr, F2 <: LogicalExpr](left: F1, right: F2)
  extends LogicalExpr2[F1, F2]("and")

case class Or[F1 <: LogicalExpr, F2 <: LogicalExpr](left: F1, right: F2)
  extends LogicalExpr2[F1, F2]("or")

case class Equals[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2]("=")

case class NotEquals[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2]("<>")

case class GreaterThan[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2](">")

case class LessThan[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2]("<")

case class GreaterThanOrEqual[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2](">=")

case class LessThanOrEqual[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparable[F1, F2])
  extends LogicalExpr2[F1, F2]("<=")

case class Like[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using IsText[F1], IsText[F2])
  extends LogicalExpr2[F1, F2]("like")

case class ILike[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using IsText[F1], IsText[F2])
  extends LogicalExpr2[F1, F2]("ilike")

case class IsSubset[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparableArray[F1, F2])
  extends LogicalExpr2[F1, F2]("<@")

case class IsSuperset[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparableArray[F1, F2])
  extends LogicalExpr2[F1, F2]("@>")

case class Overlaps[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using AreComparableArray[F1, F2])
  extends LogicalExpr2[F1, F2]("&&")

case class Contains[F1 <: Field[_], F2 <: Field[_]](left: F1, right: F2)(using CanContain[F1, F2])
  extends LogicalExpr2[F1, F2]("= any"):
  override def toString: String = s"$left = any($right)"

case class IsIn[F1 <: Field[_], S <: SelectQuery[_ <: Relations, _, _, _, _, _, _, _]](left: F1, right: S)(using CanContain[S, F1])
  extends LogicalExpr2[F1, S]("in")
