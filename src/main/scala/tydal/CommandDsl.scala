package tydal

import tydal.compiler.CommandCompiler

trait CommandDsl:
  def compile[Input](using compiler: CommandCompiler[this.type, Input]): skunk.Command[Input] =
    compiler.build(this)
