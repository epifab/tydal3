package tydal.examples

import cats.effect.{ExitCode, IO, IOApp}
import skunk.data.Arr
import skunk.{Query, Session, Strategy}
import tydal.Schema.*
import tydal.*

object BasicExample extends IOApp with SessionAware:

  // Select all artists named
  val query =
    Select
      .from(artist as "a")
      .take(_("a").*)
      .where(x => (x("a", "genres") overlaps "genres?") or (x("a", "name") like "name?"))
      .sortBy(_("a", "name"))
      .offset(0L[int8])
      .limit("limit?")
      .compile

  def run(args: List[String]): IO[ExitCode] =
    (for {
      preparedQuery <- fs2.Stream.resource(session.flatMap(_.prepareR(query)))
      artists <- preparedQuery.stream((
        "genres?" ~~> Arr(Genre.Rock, Genre.Psychedelic),
        "name?" ~~> "%Floyd",
        "limit?" ~~> 50
      ), 10)
      _ <- fs2.Stream.eval(IO(println(artists)))
    } yield ()).compile.drain.as(ExitCode.Success)
