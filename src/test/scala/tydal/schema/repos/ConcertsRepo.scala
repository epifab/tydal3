package tydal.schema.repos

import cats.Monad
import cats.Monad.ops.toAllMonadOps
import cats.Traverse.ops.toAllTraverseOps
import cats.data.OptionT
import cats.effect.Resource
import cats.effect.kernel.Concurrent
import skunk.data.Arr
import skunk.{PreparedQuery, Session}
import tydal.schema._
import tydal.schema.repos.Schema._

import java.time.Instant
import java.util.UUID

case class ConcertArtistRecord(id: UUID, headliner: Boolean)
case class TicketRecord(price: BigDecimal, currency: Currency)

case class Concert(
  begin: Instant,
  end: Instant,
  venueName: String,
  artists: List[String],
  cheapestTickets: Map[Currency, BigDecimal]
)

trait ConcertsRepo[F[_]]:
  def create(venueId: UUID, begin: Instant, end: Instant, artists: Seq[ConcertArtistRecord], tickets: Seq[TicketRecord]): F[UUID]
  def findOne(concertId: UUID): F[Option[Concert]]

object ConcertsRepo:

  private val insertConcertCommand =
    Insert
      .into(concert)
      .fields(c => (
        c("id"),
        c("venue_id"),
        c("begins_at"),
        c("ends_at")
      ))
      .compile

  private val insertConcertArtistCommand =
    Insert
      .into(concert_artist)
      .fields(ca => (
        ca("concert_id"),
        ca("artist_id"),
        ca("headliner"),
        ca("index")
      ))
      .compile

  private val insertTicketCommand =
    Insert
      .into(ticket)
      .fields(t => (
        t("id"),
        t("concert_id"),
        t("price"),
        t("currency")
      ))
      .compile

  private val selectConcertsByIdQuery =
    Select
      .from(concert as "c")
      .innerJoin(venue as "v").on(_("id") === _("c", "venue_id"))
      .take(x => (
        x("c", "id"),
        x("c", "begins_at"),
        x("c", "ends_at"),
        x("v", "name") as "venue_name"
      ))
      .sortBy(_("c", "begins_at"))
      .where(_("c", "id") anyOf "ids?")
      .compile

  val selectArtistsQuery =
    Select
      .from(concert_artist as "ca")
      .innerJoin(artist as "a").on(_("id") === _("ca", "artist_id"))
      .take(x => (x("ca", "concert_id"), x("a", "name")))
      .where(_("ca", "concert_id") anyOf "concertIds?")
      .sortBy(x => (x("ca", "concert_id"), x("ca", "index")))
      .compile

  val selectCheapestTicketsQUery =
    Select
      .from(ticket as "t")
      .take(x => (
        x("t", "concert_id"),
        x("t", "currency"),
        Min(x("t", "price")) as "min_price"
      ))
      .where(_("t", "concert_id") anyOf "concertIds?")
      .groupBy(x => (
        x("t", "concert_id"),
        x("t", "currency")
      ))
      .compile

  def apply[F[_]: Monad: Concurrent](newId: F[UUID], session: Resource[F, Session[F]]): Resource[F, ConcertsRepo[F]] =
    for {
      s <- session
      insertConcert <- s.prepare(insertConcertCommand)
      insertConcertArtist <- s.prepare(insertConcertArtistCommand)
      insertTicket <- s.prepare(insertTicketCommand)
      selectConcertById: PreparedQuery[F, "ids?" ~~> Arr[UUID], (UUID, Instant, Instant, String)] <- s.prepare(selectConcertsByIdQuery)
      selectArtists: PreparedQuery[F, "concertIds?" ~~> Arr[UUID], (UUID, String)] <- s.prepare(selectArtistsQuery)
      selectTickets: PreparedQuery[F, "concertIds?" ~~> Arr[UUID], (UUID, Currency, Option[BigDecimal])] <- s.prepare(selectCheapestTicketsQUery)

      repo = new ConcertsRepo[F] {
        override def create(venueId: UUID, begin: Instant, end: Instant, artists: Seq[ConcertArtistRecord], tickets: Seq[TicketRecord]): F[UUID] =
          s.transaction.use { _ =>
            for {
              concertId <- newId
              _ <- insertConcert.execute((
                "id" ~~> concertId,
                "venue_id" ~~> venueId,
                "begins_at" ~~> begin,
                "ends_at" ~~> end
              ))
              _ <- artists.zipWithIndex.traverse { case (a, index) =>
                insertConcertArtist.execute((
                  "concert_id" ~~> concertId,
                  "artist_id" ~~> a.id,
                  "headliner" ~~> a.headliner,
                  "index" ~~> (index + 1)
                ))
              }
              _ <- tickets.traverse { ticket =>
                for {
                  ticketId <- newId
                  _ <- insertTicket.execute((
                    "id" ~~> ticketId,
                    "concert_id" ~~> concertId,
                    "price" ~~> ticket.price,
                    "currency" ~~> ticket.currency
                  ))
                } yield ()
              }
            } yield concertId
          }

        override def findOne(concertId: UUID): F[Option[Concert]] =
          (for {
            concert <- OptionT(selectConcertById.option("ids?" ~~> Arr(concertId)))
            artists <- OptionT.whenF(concert.nonEmpty)(selectArtists.stream("concertIds?" ~~> Arr(concertId), 128).compile.toList)
            tickets <- OptionT.whenF(concert.nonEmpty)(selectTickets.stream("concertIds?" ~~> Arr(concertId), 128).compile.toList)
          } yield (Concert(
            concert(1),
            concert(2),
            concert(3),
            artists.collect { case (concertId, artistName) if concertId == concert(0) => artistName },
            tickets.collect { case (concertId, currency, Some(price)) => currency -> price }.toMap
          ))).value
       }

    } yield repo
