package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait LogicalExprFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object LogicalExprFragment:
  given alwaysTrue: LogicalExprFragment[AlwaysTrue, EmptyTuple] with
    def build(alwaysTrue: AlwaysTrue): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(None, EmptyTuple)

  given isDefined[F <: Field[_], T <: Tuple](using fragment: FieldFragment[F, T]): LogicalExprFragment[F, T] with
    def build(f: F): CompiledQueryFragment[T] = fragment.build(f).append(" is not null")

  given isNotDefined[F <: Field[_], T <: Tuple](using fragment: FieldFragment[F, T]): LogicalExprFragment[F, T] with
    def build(f: F): CompiledQueryFragment[T] = fragment.build(f).append(" is null")

  given and[E1 <: LogicalExpr, E2 <: LogicalExpr, E <: LogicalExpr2[E1, E2], T1 <: Tuple, T2 <: Tuple](
    using
    left: LogicalExprFragment[E1, T1],
    right: LogicalExprFragment[E2, T2]
  ): LogicalExprFragment[And[E1, E2], T1 Concat T2] with
    def build(e: And[E1, E2]): CompiledQueryFragment[T1 Concat T2] =
      left.build(e.left).concatenateOptional(right.build(e.right), " AND ")

  given or[E1 <: LogicalExpr, E2 <: LogicalExpr, E <: LogicalExpr2[E1, E2], T1 <: Tuple, T2 <: Tuple](
    using
    left: LogicalExprFragment[E1, T1],
    right: LogicalExprFragment[E2, T2]
  ): LogicalExprFragment[Or[E1, E2], T1 Concat T2] with
    def build(e: Or[E1, E2]): CompiledQueryFragment[T1 Concat T2] =
      left.build(e.left).concatenateOptional(right.build(e.right), " OR ")

  given comparison[F1 <: Field[_], F2 <: Field[_], E <: Comparison[F1, F2], P <: Tuple, Q <: Tuple](
    using
    left: FieldFragment[F1, P],
    right: FieldFragment[F2, Q]
  ): LogicalExprFragment[E, P Concat Q] with
    def build(filter: E): CompiledQueryFragment[P Concat Q] =
      val e1 = left.build(filter.left)
      val e2 = right.build(filter.right)
      filter match
        case _: Equals[_, _] => e1.concatenateRequired(e2, " = ")
        case _: NotEquals[_, _] => e1.concatenateRequired(e2, " != ")
        case _: GreaterThan[_, _] => e1.concatenateRequired(e2, " > ")
        case _: LessThan[_, _] => e1.concatenateRequired(e2, " < ")
        case _: GreaterThanOrEqual[_, _] => e1.concatenateRequired(e2, " >= ")
        case _: LessThanOrEqual[_, _] => e1.concatenateRequired(e2, " <= ")
        case _: Like[_, _] => e1.concatenateRequired(e2, " LIKE ")
        case _: ILike[_, _] => e1.concatenateRequired(e2, " ILIKE ")
        case _: IsSubset[_, _] => e1.concatenateRequired(e2, " <@ ")
        case _: IsSuperset[_, _] => e1.concatenateRequired(e2, " @> ")
        case _: Overlaps[_, _] => e1.concatenateRequired(e2, " && ")
        case _: AnyOf[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " = ANY")

  given in[F <: Field[_], S <: SelectQuery[_, _, _, _, _, _, _, _], T1 <: Tuple, T2 <: Tuple](
    using
    left: FieldFragment[F, T1],
    right: QueryCompiler[S, T2, _]
  ): LogicalExprFragment[In[F, S], T1 Concat T2] with
    def build(e: In[F, S]): CompiledQueryFragment[T1 Concat T2] =
      left.build(e.left).concatenateRequired(CompiledQueryFragment(right.build(e.right)).wrap("(", ")"), " IN ")

  given notIn[F <: Field[_], S <: SelectQuery[_, _, _, _, _, _, _, _], T1 <: Tuple, T2 <: Tuple](
    using
    left: FieldFragment[F, T1],
    right: QueryCompiler[S, T2, _]
  ): LogicalExprFragment[NotIn[F, S], T1 Concat T2] with
    def build(e: NotIn[F, S]): CompiledQueryFragment[T1 Concat T2] =
      left.build(e.left).concatenateRequired(CompiledQueryFragment(right.build(e.right)).wrap("(", ")"), " NOT IN ")
