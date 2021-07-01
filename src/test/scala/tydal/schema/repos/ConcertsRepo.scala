package tydal.schema.repos

import cats.Monad
import cats.Monad.ops.toAllMonadOps
import cats.Traverse.ops.toAllTraverseOps
import cats.effect.Resource
import cats.effect.kernel.Concurrent
import skunk.Session
import Schema._
import tydal.schema._

import java.time.Instant
import java.util.UUID

case class ConcertArtistRecord(id: UUID, headliner: Boolean)
case class TicketRecord(price: BigDecimal, currency: Currency)

case class Concert(
  begin: Instant,
  end: Instant,
  venueName: String,
  artists: Map[Int, String],
  cheapestTickets: Map[Currency, BigDecimal]
)

trait ConcertsRepo[F[_]]:
  def create(venueId: UUID, begin: Instant, end: Instant, artists: Seq[ConcertArtistRecord], tickets: Seq[TicketRecord]): F[UUID]
  def findOne(concertId: UUID): F[Option[Concert]]

object ConcertsRepo:

  private val insertConcert =
    Insert
      .into(concert)
      .fields(c => (
        c("id"),
        c("venue_id"),
        c("begins_at"),
        c("ends_at")
      ))
      .compile


  private val insertConcertArtist =
    Insert
      .into(concert_artist)
      .fields(ca => (
        ca("concert_id"),
        ca("artist_id"),
        ca("headliner"),
        ca("index")
      ))
      .compile

  private val insertTicket =
    Insert
      .into(ticket)
      .fields(t => (
        t("id"),
        t("concert_id"),
        t("price"),
        t("currency")
      ))
      .compile

  val selectConcert =
    Select
      .from(concert as "c")
      .innerJoin(venue as "v").on(_("id") === _("c", "venue_id"))
      .innerJoin(concert_artist as "ca").on(_("concert_id") === _("c", "id"))
      .innerJoin(artist as "a").on(_("id") === _("ca", "artist_id"))
      .leftJoin(
        Select
          .from(ticket as "t")
          .take(x => (
            x("t", "concert_id") as "cid",
            x("t", "currency") as "currency",
            Min(x("t", "price")) as "min_price"
          ))
          .groupBy(x => (x("cid"), x("currency")))
          .as("tx")
      ).on(_("cid") === _("c", "id"))
      .take(x => (
        x("c", "id"),
        x("c", "begins_at"),
        x("c", "ends_at"),
        x("v", "name") as "venue_name",
        x("ca", "index") as "artist_index",
        x("a", "name") as "artist_name",
        x("tx", "currency"),
        x("tx", "min_price")
      ))
      .where(_("c", "id") === "id?")
      .sortBy(x => (x("c", "begins_at"), x("ca", "index")))
      .compile

  def apply[F[_]: Monad: Concurrent](newId: F[UUID], session: Resource[F, Session[F]]): Resource[F, ConcertsRepo[F]] =
    for {
      s <- session
      insertConcertStatement <- s.prepare(insertConcert)
      insertConcertArtistStatement <- s.prepare(insertConcertArtist)
      insertTicketStatement <- s.prepare(insertTicket)
      selectConcertStatement <- s.prepare(selectConcert)

      repo = new ConcertsRepo[F] {
        override def create(venueId: UUID, begin: Instant, end: Instant, artists: Seq[ConcertArtistRecord], tickets: Seq[TicketRecord]): F[UUID] =
          // todo: uncommentin the following I get could not find an instance of Monad[F], why?
          // s.transaction.use { _ =>
            for {
              concertId <- newId
              _ <- insertConcertStatement.execute((
                "id" ~~> concertId,
                "venue_id" ~~> venueId,
                "begins_at" ~~> begin,
                "ends_at" ~~> end
              ))
              _ <- artists.zipWithIndex.traverse { case (a, index) =>
                insertConcertArtistStatement.execute((
                  "concert_id" ~~> concertId,
                  "artist_id" ~~> a.id,
                  "headliner" ~~> a.headliner,
                  "index" ~~> (index + 1)
                ))
              }
              _ <- tickets.traverse { ticket =>
                for {
                  ticketId <- newId
                  _ <- insertTicketStatement.execute((
                    "id" ~~> ticketId,
                    "concert_id" ~~> concertId,
                    "price" ~~> ticket.price,
                    "currency" ~~> ticket.currency
                  ))
                } yield ()
              }
            } yield concertId
          // }

        override def findOne(concertId: UUID): F[Option[Concert]] =
          selectConcertStatement
            .stream(("id?" ~~> concertId) *: EmptyTuple, 128)
            .fold[Option[Concert]](None) {
              case (None, (id, begin, end, venueName, artistIndex, artistName, currency, price)) =>
                Some(Concert(
                  begin = begin,
                  end = end,
                  venueName = venueName,
                  artists = Map(artistIndex -> artistName),
                  cheapestTickets = (for {
                    c <- currency
                    p <- price
                  } yield Map(c -> p)).getOrElse(Map.empty)
                ))

              case (Some(concert), (_, _, _, _, artistIndex, artistName, currency, price)) =>
                val artists: Map[Int, String] = concert.artists ++ Map(artistIndex -> artistName)

                val tickets: Map[Currency, BigDecimal] = concert.cheapestTickets ++ (for {
                  c <- currency
                  p <- price
                } yield Map(c -> p)).getOrElse(Map.empty)

                Some(concert.copy(
                  artists = artists,
                  cheapestTickets = tickets
                ))
            }
            .compile
            .last
            .map(_.flatten)
       }

    } yield repo
