package tydal.test

import cats.effect.unsafe.IORuntime
import org.scalatest.matchers._
import skunk.Query
import tydal.SessionAware

trait IntegrationTesting extends SessionAware with should.Matchers:
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  def testQuery[A, B](query: Query[A, B], expectedSql: String, input: A): List[B] =
    query.sql shouldBe expectedSql
    session
      .flatMap(_.prepare(query))
      .use(_.stream(input, 4).compile.toList)
      .unsafeRunSync()
