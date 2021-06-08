package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryFragmentCompiler[-Target, Input <: Tuple]:
  def build(x: Target): CompiledQueryFragment[Input]


case class CompiledQueryFragment[Input <: Tuple](sql: Option[String], input: Input):

  def `++`(other: String): CompiledQueryFragment[Input] = append(" " + other)

  def `++`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, "")

  def `+ +`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+,+`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): CompiledQueryFragment[Input] = map(before + _ + after)

  def append(after: String): CompiledQueryFragment[Input] = map(_ + after)

  def prepend(before: String): CompiledQueryFragment[Input] = map(before + _)

  def map(f: String => String): CompiledQueryFragment[Input] = CompiledQueryFragment(sql.map(f), input)

  def mapInput[I2 <: Tuple](f: Input => I2): CompiledQueryFragment[I2] = CompiledQueryFragment(sql, f(input))

  def concatenateOptional[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      Seq(sql, other.sql)
        .flatten
        .reduceOption(_ ++ separator ++ _),
      input ++ other.input
    )

  def concatenateRequired[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      for {
        s1 <- sql
        s2 <- other.sql
      } yield s1 + separator + s2,
      input ++ other.input
    )

  def getOrElse[Output <: Tuple](default: String, output: Output): CompiledQuery[Input, Output] =
    CompiledQuery(sql.getOrElse(default), input, output)

  def orElse(s: Option[String]): CompiledQueryFragment[Input] =
    new CompiledQueryFragment(sql.orElse(s), input)

  def get[Output <: Tuple](output: Output): CompiledQuery[Input, Output] =
    CompiledQuery(sql.get, input, output)

object CompiledQueryFragment:
  def apply(sql: String): CompiledQueryFragment[EmptyTuple] =
    CompiledQueryFragment(Some(sql), EmptyTuple)

  def apply[P <: Placeholder[_]](sql: String, placeholder: P): CompiledQueryFragment[P *: EmptyTuple] =
    CompiledQueryFragment(Some(sql), placeholder *: EmptyTuple)

  def apply[Input <: Tuple](query: CompiledQuery[Input, _]): CompiledQueryFragment[Input] =
    CompiledQueryFragment(Some(query.sql), query.input)
