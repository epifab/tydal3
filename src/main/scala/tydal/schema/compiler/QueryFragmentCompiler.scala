package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat
import cats.data.State

trait QueryFragmentCompiler[-Target, Input <: Tuple]:
  def build(x: Target): CompiledQueryFragment[Input]

case class CompiledQueryFragment[Input <: Tuple](parts: List[String | State[Int, String]], input: Input):

  def sql: String = parts.foldLeft("") {
    case (x, s: String) => x + s
    case (x, s: State[_, _]) => x + "?"
  }

  def `++`(other: String): CompiledQueryFragment[Input] = append(" " + other)

  def `++`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+ +`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+,+`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else (before :: parts) :+ after, input)

  def append(after: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else parts :+ after, input)

  def prepend(before: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else before :: parts, input)

  def concatenateOptional[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (false, false) => parts ++ (separator :: other.parts)
        case _ => parts ++ other.parts,
      input ++ other.input
    )

  def concatenateRequired[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (true, true) => parts ++ (separator :: other.parts)
        case _ => Nil,
      input ++ other.input
    )

  def orElse(s: String): CompiledQueryFragment[Input] =
    if (parts.isEmpty) CompiledQueryFragment(s :: Nil, input) else this

object CompiledQueryFragment:
  def empty: CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(Nil, EmptyTuple)
  def apply(const: String): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(const :: Nil, EmptyTuple)
