package tydal

import cats.effect.{IO, Resource}
import natchez.Trace.Implicits.noop
import skunk.{Session, Strategy}

trait SessionAware:
  val session: Resource[IO, Session[IO]] =
    Session.single(
      host     = "localhost",
      port     = 5432,
      user     = "root",
      password = Some("p4ssw0rd"),
      database = "tydal",
      strategy = Strategy.SearchPath
    )
