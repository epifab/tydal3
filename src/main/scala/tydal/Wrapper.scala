package tydal

class ExprWrapper[T, +F <: Field[T]](val expr: F) extends Field[T]:
  given dbType: DbType[T] = expr.dbType

class LogicalExprWrapper[+E <: LogicalExpr](val expr: E) extends LogicalExpr

trait Wrapper:
  def apply[T, F <: Arithemtic[_, _, T]](expr: F): ExprWrapper[T, F] = ExprWrapper(expr)
  def apply[E <: LogicalExpr](expr: E): LogicalExprWrapper[E] = LogicalExprWrapper(expr)

object << extends Wrapper
