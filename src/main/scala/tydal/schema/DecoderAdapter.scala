package tydal.schema

import cats.data.State
import skunk.Decoder
import skunk.Void
import skunk.data.Type

import scala.Tuple.Concat

trait DecoderAdapter[-T, U <: Tuple]:
  def apply(t: T): Decoder[U]

object DecoderAdapter:
  def apply[A, B <: Tuple](a: A)(using adapter: DecoderAdapter[A, B]): Decoder[B] = adapter(a)

  // todo: next doens't work, because of https://github.com/lampepfl/dotty/issues/12940
//  given field[T, U]: DecoderAdapter[Field.Aux[T, U], U *: EmptyTuple] with
//    def apply(t: Field.Aux[T, U]): Decoder[U *: EmptyTuple] =
//      t.decoder.map { case value => value *: EmptyTuple }

  given field[T, U](using dbType: DbType.Aux[T, U]): DecoderAdapter[Field[T], U *: EmptyTuple] with
    def apply(t: Field[T]): Decoder[U *: EmptyTuple] =
      dbType.codec.asDecoder.map { case value => value *: EmptyTuple }

  given empty: DecoderAdapter[EmptyTuple, EmptyTuple] with
    def apply(t: EmptyTuple): Decoder[EmptyTuple] =
      Void.codec.asDecoder.map { _ => EmptyTuple }

  given nonEmpty[H, HDec <: Tuple, T <: Tuple, TDec <: Tuple](
    using
    head: DecoderAdapter[H, HDec],
    tail: DecoderAdapter[T, TDec]
  ): DecoderAdapter[H *: T, Concat[HDec, TDec]] with
    def apply(t: H *: T): Decoder[Concat[HDec, TDec]] = new Decoder[Concat[HDec, TDec]]:
      override def decode(offset: Int, ss: List[Option[String]]): Either[Decoder.Error, Concat[HDec, TDec]] =
        val headDecoder: Decoder[HDec] = head(t.head)
        val tailDecoder: Decoder[TDec] = tail(t.tail)
        val (sa, sb) = ss.splitAt(headDecoder.types.length)
        for {
          h <- headDecoder.decode(offset, sa)
          t <- tailDecoder.decode(offset, sb)
        } yield (h ++ t)
      override val types: List[Type] = head(t.head).types ++ tail(t.tail).types
