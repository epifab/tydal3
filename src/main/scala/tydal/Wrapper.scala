package tydal

class ExprWrapper[T, +F <: Field[T]](val expr: F) extends Field[T]:
  given dbType: DbType[T] = expr.dbType

trait Wrapper:
  def apply[T, F <: Arithemtic[_, _, T]](expr: F): ExprWrapper[T, F] = ExprWrapper(expr)

object << extends Wrapper
