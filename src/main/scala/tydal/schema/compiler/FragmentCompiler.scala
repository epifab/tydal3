package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat
import cats.data.State

trait FragmentCompiler[-Target, Input <: Tuple]:
  def build(x: Target): CompiledFragment[Input]

case class CompiledFragment[Input <: Tuple](parts: List[String | State[Int, String]], input: Input):

  def sql: String = parts.foldLeft("") {
    case (x, s: String) => x + s
    case (x, s: State[_, _]) => x + "?"
  }

  def `++`(other: String): CompiledFragment[Input] = append(" " + other)

  def `++`[I2 <: Tuple](other: CompiledFragment[I2]): CompiledFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+ +`[I2 <: Tuple](other: CompiledFragment[I2]): CompiledFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+,+`[I2 <: Tuple](other: CompiledFragment[I2]): CompiledFragment[Input Concat I2] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): CompiledFragment[Input] = CompiledFragment(if (parts.isEmpty) Nil else (before :: parts) :+ after, input)

  def append(after: String): CompiledFragment[Input] = CompiledFragment(if (parts.isEmpty) Nil else parts :+ after, input)

  def prepend(before: String): CompiledFragment[Input] = CompiledFragment(if (parts.isEmpty) Nil else before :: parts, input)

  def concatenateOptional[I2 <: Tuple](other: CompiledFragment[I2], separator: String): CompiledFragment[Input Concat I2] =
    CompiledFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (false, false) => parts ++ (separator :: other.parts)
        case _ => parts ++ other.parts,
      input ++ other.input
    )

  def concatenateRequired[I2 <: Tuple](other: CompiledFragment[I2], separator: String): CompiledFragment[Input Concat I2] =
    CompiledFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (true, true) => parts ++ (separator :: other.parts)
        case _ => Nil,
      input ++ other.input
    )

  def orElse(s: String): CompiledFragment[Input] =
    if (parts.isEmpty) CompiledFragment(s :: Nil, input) else this

object CompiledFragment:
  def empty: CompiledFragment[EmptyTuple] = CompiledFragment(Nil, EmptyTuple)
  def apply(const: String): CompiledFragment[EmptyTuple] = CompiledFragment(const :: Nil, EmptyTuple)