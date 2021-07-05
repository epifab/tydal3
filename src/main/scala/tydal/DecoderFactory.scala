package tydal

import cats.data.State
import skunk.Decoder
import skunk.Void
import skunk.data.Type
import skunk.syntax.all._

import scala.Tuple.Concat

trait DecoderFactory[-T, U]:
  def apply(t: T): Decoder[U]

object DecoderFactory:
  def apply[A, B](a: A)(using adapter: DecoderFactory[A, B]): Decoder[B] = adapter(a)

  // todo: next doens't work, because of https://github.com/lampepfl/dotty/issues/12940
//  given field[T, U]: DecoderFactory[Field.Aux[T, U], U *: EmptyTuple] with
//    def apply(t: Field.Aux[T, U]): Decoder[U *: EmptyTuple] =
//      t.decoder.map { case value => value *: EmptyTuple }

  given field[T, U](using dbType: DbType.Aux[T, U]): DecoderFactory[Field[T], U] with
    def apply(t: Field[T]): Decoder[U] =
      dbType.codec

  given empty: DecoderFactory[EmptyTuple, EmptyTuple] with
    def apply(t: EmptyTuple): Decoder[EmptyTuple] =
      Void.codec.asDecoder.map { _ => EmptyTuple }

  given nonEmpty[H, HDec, T <: Tuple, TDec <: Tuple](
    using
    head: DecoderFactory[H, HDec],
    tail: DecoderFactory[T, TDec]
  ): DecoderFactory[H *: T, HDec *: TDec] with
    def apply(t: H *: T): Decoder[HDec *: TDec] = head(t.head) *: tail(t.tail)
