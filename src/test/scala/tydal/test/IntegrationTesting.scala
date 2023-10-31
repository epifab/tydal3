package tydal.test

import cats.effect.unsafe.IORuntime
import cats.kernel.Eq
import org.scalatest.Assertion
import org.scalatest.matchers.*
import skunk.{Query, Void}
import tydal.SessionAware

import java.time.{Instant, LocalDate}
import java.util.UUID

trait IntegrationTesting extends SessionAware with should.Matchers:

  given [H, T <: Tuple](using h: Eq[H], t: Eq[T]): Eq[H *: T] with
    override def eqv(a: H *: T, b: H *: T) = h.eqv(a.head, b.head) && t.eqv(a.tail, b.tail)

  given Eq[BigDecimal] with
    override def eqv(x: BigDecimal, y: BigDecimal): Boolean = x == y

  given Eq[UUID] with
    override def eqv(x: UUID, y: UUID): Boolean = x == y

  given Eq[Instant] with
    override def eqv(x: Instant, y: Instant): Boolean = x == y

  given Eq[LocalDate] with
    override def eqv(x: LocalDate, y: LocalDate): Boolean = x == y

  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def testQuery[A, B](query: Query[A, B], expectedSql: String, input: A): List[B] =
    query.sql shouldBe expectedSql
    session
      .flatMap(_.prepareR(query))
      .use(_.stream(input, 4).compile.toList)
      .unsafeRunSync()

  def testUnique[A](query: Query[Void, A], expectedQuery: String, expectedResult: A)(using eq: Eq[A]): Assertion =
    query.sql shouldBe expectedQuery
    assertEq(session.flatMap(_.prepareR(query)).use(_.unique(Void)).unsafeRunSync(), expectedResult)

  def assertEq[A](actual: A, expected: A)(using eq: Eq[A]): Assertion =
    assert(eq.eqv(actual, expected), s"$actual was not $expected")
