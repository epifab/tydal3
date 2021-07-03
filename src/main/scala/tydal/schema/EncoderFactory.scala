package tydal.schema

import cats.data.State
import skunk.Encoder
import skunk.Void
import skunk.data.Type
import cats.implicits.catsSyntaxTuple2Semigroupal

import Tuple.Concat

trait EncoderFactory[-T, U <: Tuple]:
  def apply(t: T): Encoder[U]

object EncoderFactory:
  def apply[A, B <: Tuple](a: A)(using adapter: EncoderFactory[A, B]): skunk.Encoder[B] = adapter(a)

  given placeholder[A <: String with Singleton, T, U](using dbType: DbType.Aux[T, U]): EncoderFactory[Placeholder[A, T], (A KeyValue U) *: EmptyTuple] with
    def apply(placeholder: Placeholder[A, T]): Encoder[(A KeyValue dbType.Out) *: EmptyTuple] =
      dbType.codec.asEncoder.contramap { case kv *: EmptyTuple => kv.value }

  given const[T]: EncoderFactory[Const[T], EmptyTuple] with
    def apply(const: Const[T]): Encoder[EmptyTuple] =
      const.dbType.codec.asEncoder.contramap(_ => const.value)

  given empty: EncoderFactory[EmptyTuple, EmptyTuple] with
    def apply(et: EmptyTuple): Encoder[EmptyTuple] = Void.codec.asEncoder.contramap[EmptyTuple](_ => Void)

  given nonEmpty[H, HEnc <: Tuple, T <: Tuple, TEnc <: Tuple](
    using
    head: EncoderFactory[H, HEnc],
    tail: EncoderFactory[T, TEnc],
    split: Split[Concat[HEnc, TEnc], HEnc, TEnc]
  ): EncoderFactory[H *: T, Concat[HEnc, TEnc]] with
    def apply(t: H *: T): Encoder[Concat[HEnc, TEnc]] = new Encoder[Concat[HEnc, TEnc]]:
      override def encode(ab: Concat[HEnc, TEnc]): List[Option[String]] =
        val (a, b) = split(ab)
        head(t.head).encode(a) ++ tail(t.tail).encode(b)
      override val types: List[Type] = head(t.head).types ++ tail(t.tail).types
      override val sql: State[Int, String] = (head(t.head).sql, tail(t.tail).sql).mapN((a, b) => s"$a, $b")


trait Split[Src <: Tuple, A <: Tuple, B <: Tuple]:
  def apply(ab: Src): (A, B)

object Split:
  given secondHalf[A <: Tuple]: Split[A, EmptyTuple, A] with
    def apply(ab: A): (EmptyTuple, A) = (EmptyTuple, ab)

  given firstHalf[Head, ATail <: Tuple, Tail <: Tuple, B <: Tuple](
    using
    tailSplit: Split[Tail, ATail, B]
  ): Split[Head *: Tail, Head *: ATail, B] with
    def apply(ab: Head *: Tail): (Head *: ATail, B) =
      val xx: (ATail, B) = tailSplit(ab.tail)
      (ab.head *: xx._1, xx._2)


object SplitSpec:
  summon[Split[(1, 2), EmptyTuple, (1, 2)]]
  summon[Split[(1, 2), (1, 2), EmptyTuple]]
  summon[Split[EmptyTuple, EmptyTuple, EmptyTuple]]
  summon[Split[(1, 2, 3, 4, 5), (1, 2), (3, 4, 5)]]
  summon[Split[Concat[(1, 2), (3, 4, 5)], (1, 2), (3, 4, 5)]]
