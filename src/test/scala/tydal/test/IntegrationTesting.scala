package tydal.test

import cats.effect._
import cats.effect.unsafe.IORuntime
import natchez.Trace.Implicits.noop
import skunk.{Session, Strategy}

trait IntegrationTesting:
  implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

  val session: Resource[IO, Session[IO]] =
    Session.single(
      host     = "localhost",
      port     = 5432,
      user     = "root",
      password = Some("p4ssw0rd"),
      database = "tydal",
      strategy = Strategy.SearchPath
    )

