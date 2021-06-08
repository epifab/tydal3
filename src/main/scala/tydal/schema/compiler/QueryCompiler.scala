package tydal.schema.compiler

trait QueryCompiler[Query, Input <: Tuple, Output <: Tuple]:
  def build(query: Query): CompiledQuery[Input, Output]

case class CompiledQuery[Input <: Tuple, Output <: Tuple](sql: String, input: Input, output: Output)
