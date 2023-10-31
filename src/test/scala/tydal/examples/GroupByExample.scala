package tydal.examples

import cats.effect.{ExitCode, IO, IOApp}
import skunk.data.Arr
import skunk.{Query, Session, Strategy}
import tydal.Schema.*
import tydal.*

object GroupByExample extends IOApp with SessionAware:

  // Select artist names who played at least 2 concerts
  val query =
    Select
      .from(artist as "a")
      .leftJoin(concert_artist as "ca").on(_("artist_id") === _("a", "id"))
      .take(_("a", "name"))
      .groupBy(_("a", "name"))
      .having(x => Count(x("ca", "concert_id")) > 2L[int8])
      .limit(50[int4])
      .compile

  def run(args: List[String]): IO[ExitCode] =
    (for {
      preparedQuery <- fs2.Stream.resource(session.flatMap(_.prepareR(query)))
      artists <- preparedQuery.stream(skunk.Void, 10)
      _ <- fs2.Stream.eval(IO(println(artists)))
    } yield ()).compile.drain.as(ExitCode.Success)
