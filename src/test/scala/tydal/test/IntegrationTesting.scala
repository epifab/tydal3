package tydal.test

import cats.effect.unsafe.IORuntime
import org.scalatest.Assertion
import org.scalatest.matchers._
import skunk.{Query, Void}
import tydal.SessionAware

trait IntegrationTesting extends SessionAware with should.Matchers:
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def testQuery[A, B](query: Query[A, B], expectedSql: String, input: A): List[B] =
    query.sql shouldBe expectedSql
    session
      .flatMap(_.prepare(query))
      .use(_.stream(input, 4).compile.toList)
      .unsafeRunSync()

  def testUnique[A](query: Query[Void, A], expectedQuery: String, expectedResult: A): Assertion =
    query.sql shouldBe expectedQuery
    session.flatMap(_.prepare(query)).use(_.unique(Void)).unsafeRunSync() shouldBe expectedResult
