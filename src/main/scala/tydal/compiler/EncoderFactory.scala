package tydal
package compiler

import cats.data.State
import cats.implicits.catsSyntaxTuple2Semigroupal
import skunk.data.Type
import skunk.syntax.all._
import skunk.{Encoder, Void}


trait EncoderFactory[-T, U]:
  def apply(t: T): Encoder[U]

object EncoderFactory:
  def apply[A, B](a: A)(using factory: EncoderFactory[A, B]): skunk.Encoder[B] = factory(a)

  trait CombinedEncoder[E1, E2, E]:
    def apply(e1: Encoder[E1], e2: Encoder[E2]): Encoder[E]

  trait DefaultCombiner:
    given tuple2[A, B]: CombinedEncoder[A, B, (A, B)] with
      def apply(e1: Encoder[A], e2: Encoder[B]): Encoder[(A, B)] = new Encoder[(A, B)]:
        override def encode(ab: (A, B)): List[Option[String]] = e1.encode(ab(0)) ++ e2.encode(ab(1))
        override val types: List[Type] = e1.types ++ e2.types
        override val sql: State[Int, String] = (e1.sql, e2.sql).mapN((a, b) => s"$a, $b")

  trait TupleCombiner extends DefaultCombiner:
    given tuple3[A, B <: Tuple]: CombinedEncoder[A, B, A *: B] with
      def apply(e1: Encoder[A], e2: Encoder[B]): Encoder[A *: B] = new Encoder[A *: B]:
        override def encode(ab: A *: B): List[Option[String]] = e1.encode(ab.head) ++ e2.encode(ab.tail)
        override val types: List[Type] = e1.types ++ e2.types
        override val sql: State[Int, String] = (e1.sql, e2.sql).mapN((a, b) => s"$a, $b")

  trait VoidLeftCombiner extends TupleCombiner:
    given unitLeft[A]: CombinedEncoder[A, Void, A] with
      def apply(e1: Encoder[A], e2: Encoder[Void]): Encoder[A] = new Encoder[A]:
        override def encode(a: A): List[Option[String]] = e1.encode(a) ++ e2.encode(Void)
        override val types: List[Type] = e1.types ++ e2.types
        override val sql: State[Int, String] = (e1.sql, e2.sql).mapN((a, b) => s"$a, $b")

  object CombinedEncoder extends VoidLeftCombiner:
    given unitRight[A]: CombinedEncoder[Void, A, A] with
      def apply(e1: Encoder[Void], e2: Encoder[A]): Encoder[A] = new Encoder[A]:
        override def encode(a: A): List[Option[String]] = e1.encode(Void) ++ e2.encode(a)
        override val types: List[Type] = e1.types ++ e2.types
        override val sql: State[Int, String] = (e1.sql, e2.sql).mapN((a, b) => s"$a, $b")

  given placeholder[A <: String with Singleton, T, U](using dbType: DbType.Aux[T, U]): EncoderFactory[Placeholder[A, T], (A KeyValue U)] with
    def apply(placeholder: Placeholder[A, T]): Encoder[(A KeyValue dbType.Out)] =
      dbType.codec.asEncoder.contramap(_.value)

  given const[T]: EncoderFactory[Const[T], Void] with
    def apply(const: Const[T]): Encoder[Void] =
      const.dbType.codec.asEncoder.contramap(_ => const.value)

  given empty: EncoderFactory[EmptyTuple, Void] with
    def apply(et: EmptyTuple): Encoder[Void] = Void.codec.asEncoder.contramap(_ => Void)

  given nonEmpty[H, T <: Tuple, HEnc, TEnc, Enc](
    using
    head: EncoderFactory[H, HEnc],
    tail: EncoderFactory[T, TEnc],
    combined: CombinedEncoder[HEnc, TEnc, Enc]
  ): EncoderFactory[H *: T, Enc] with
    def apply(t: H *: T): Encoder[Enc] =
      combined(head(t.head), tail(t.tail))
