package tydal.schema.compiler

import tydal.schema._
import Tuple.Concat

trait QueryFragmentCompiler[-Target, Input <: Tuple]:
  def build(x: Target): CompiledQueryFragment[Input]

sealed trait Questionmark:
  override val toString: String = "?"

object Questionmark extends Questionmark

case class CompiledQueryFragment[Input <: Tuple](parts: List[String | Questionmark], codecs: Input):

  def sql: String = parts.foldLeft("") {
    case (x, s: String) => x + s
    case (x, Questionmark) => x + "?"
  }

  def `++`(other: String): CompiledQueryFragment[Input] = append(" " + other)

  def `++`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+ +`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, " ")

  def `+,+`[I2 <: Tuple](other: CompiledQueryFragment[I2]): CompiledQueryFragment[Input Concat I2] =
    concatenateOptional(other, ", ")

  def wrap(before: String, after: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else (before :: parts) :+ after, codecs)

  def append(after: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else parts :+ after, codecs)

  def prepend(before: String): CompiledQueryFragment[Input] = CompiledQueryFragment(if (parts.isEmpty) Nil else before :: parts, codecs)

  def concatenateOptional[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (false, false) => parts ++ (separator :: other.parts)
        case _ => parts ++ other.parts,
      codecs ++ other.codecs
    )

  def concatenateRequired[I2 <: Tuple](other: CompiledQueryFragment[I2], separator: String): CompiledQueryFragment[Input Concat I2] =
    CompiledQueryFragment(
      (parts.isEmpty, other.parts.isEmpty) match
        case (true, true) => parts ++ (separator :: other.parts)
        case _ => Nil,
      codecs ++ other.codecs
    )

  def orElse(s: String): CompiledQueryFragment[Input] =
    if (parts.isEmpty) CompiledQueryFragment(s :: Nil, codecs) else this

object CompiledQueryFragment:
  def empty: CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(Nil, EmptyTuple)
  def apply(const: String): CompiledQueryFragment[EmptyTuple] = CompiledQueryFragment(const :: Nil, EmptyTuple)
