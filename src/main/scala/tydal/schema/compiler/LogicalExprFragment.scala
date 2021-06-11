package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait LogicalExprFragment[-T, I <: Tuple] extends QueryFragmentCompiler[T, I]

object LogicalExprFragment:
  given whereAlwaysTrue: LogicalExprFragment[AlwaysTrue, EmptyTuple] with
    def build(alwaysTrue: AlwaysTrue): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(None, EmptyTuple)

//  given whereFilterOption[F <: Filter, P <: Tuple, Q <: Tuple](
//    using
//    where: LogicalExprFragment[F, P],
//    literalOptions: LiteralOptions[P, Q]
//  ): LogicalExprFragment[FilterOption[F], Q] =
//  instance((filter: FilterOption[F]) => filter.filter match {
//    case Some(e) => where.build(e).mapPlaceholders(literalOptions.build)
//    case None => CompiledQueryFragment(None, literalOptions.empty)
//  })

  given expr1[F, E <: LogicalExpr1[F], P <: Tuple](using builder: LogicalExprFragment[F, P]): LogicalExprFragment[E, P] with
    def build(filter: E): CompiledQueryFragment[P] =
      val fragment = builder.build(filter.expr)
      filter match
        case _: IsDefined[_] => fragment ++ " IS NOT NULL"
        case _: IsNotDefined[_] => fragment ++ " IS NULL"

  given expr2[F1, F2, E <: LogicalExpr2[F1, F2], P <: Tuple, Q <: Tuple] (
    using
    left: LogicalExprFragment[F1, P],
    right: LogicalExprFragment[F2, Q]
  ): LogicalExprFragment[E, P Concat Q] with
    def build(filter: E): CompiledQueryFragment[P Concat Q] =
      val e1 = left.build(filter.left)
      val e2 = right.build(filter.right)

      filter match
        case _: And[_, _] => e1.concatenateOptional(e2, " AND ")
        case _: Or[_, _] => e1.concatenateOptional(e2, " OR ").wrap("(", ")")
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
        case _: Contains[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " = ANY")
        case _: IsIn[_, _] => e1.concatenateRequired(e2.wrap("(", ")"), " IN ")

  given field[F <: Field[_], P <: Tuple](using builder: FieldFragment[F, P]): LogicalExprFragment[F, P] with
    def build(field: F): CompiledQueryFragment[P] = builder.build(field)

  given whereSubQuery[S, P <: Tuple](using builder: QueryCompiler[S, P, _]): LogicalExprFragment[S, P] with
    def build(query: S): CompiledQueryFragment[P] = CompiledQueryFragment(builder.build(query))
