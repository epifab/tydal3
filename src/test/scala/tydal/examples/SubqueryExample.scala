package tydal.examples

import cats.effect.{ExitCode, IO, IOApp}
import skunk.data.Arr
import skunk.{Query, Session, Strategy}
import tydal.Schema._
import tydal._

object SubqueryExample extends IOApp with SessionAware:

  val query =
    Select
      .from(artist as "a")
      .leftJoin(
        Select
          .from(concert_artist as "ca")
          .innerJoin(concert as "c").on(_("id") === _("ca", "concert_id"))
          .innerJoin(ticket as "t").on(_("concert_id") === _("c", "id"))
          .groupBy(_("ca", "artist_id"))
          .take(x => (x("ca", "artist_id"), Max(x("c", "begins_at")) as "begins_at"))
          .where(x => (x("t", "currency") === "currency?") and (x("t", "price") <= "maxPrice?"))
          .as("last_concert")
      ).on(_("artist_id") === _("a", "id"))
      .take(_("last_concert", "begins_at"))
      .where(_("a", "name") === "artistName?")
      .sortBy(_("a", "name"))
      .compile

  // Select the date of the last Pink Floyd concert priced under 20$
  def run(args: List[String]): IO[ExitCode] =
    (for {
      preparedQuery <- fs2.Stream.resource(session.flatMap(_.prepare(query)))
      artists <- preparedQuery.stream((
        "currency?" ~~> Currency.USD,
        "maxPrice?" ~~> BigDecimal(20),
        "artistName?" ~~> "Pink Floyd"
      ), 10)
      _ <- fs2.Stream.eval(IO(println(artists)))
    } yield ()).compile.drain.as(ExitCode.Success)
