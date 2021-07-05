package tydal.test

import cats.Monad
import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.implicits._
import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk._
import skunk.codec.all._
import skunk.data.Arr
import skunk.implicits._
import tydal.test.repos.Schema._
import tydal.test.repos._

import java.time.{Instant, LocalDate}
import java.util.UUID


class RepositoryExampleSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  val repos = (for {
    artistsRepo <- ArtistsRepo(IO(UUID.randomUUID()), session)
    venuesRepo <- VenuesRepo(IO(UUID.randomUUID()), session)
    concertsRepo <- ConcertsRepo(IO(UUID.randomUUID()), session)
  } yield ((artistsRepo, venuesRepo, concertsRepo)))

  "A bunch of semi-complex queries can run" in {
    val expectedConcert = Concert(
      Instant.parse("2015-03-01T20:00:00Z"),
      Instant.parse("2015-03-02T01:00:00Z"),
      "Roundhouse",
      List("Radiohead", "Caribou"),
      Map(
        Currency.GBP -> 15.5,
        Currency.USD -> 90.0
      )
    )

    repos.use { case (artists, venues, concerts) =>
      for {
        radiohead <- artists.create("Radiohead", List(Genre.Rock, Genre.Electronic, Genre.Psychedelic))
        caribou <- artists.create("Caribou", List(Genre.Electronic))
        roundhouse <- venues.create(expectedConcert.venueName, Some("London"))
        concertId <- concerts.create(
          roundhouse,
          expectedConcert.begin,
          expectedConcert.end,
          List(
            ConcertArtistRecord(radiohead, headliner = true),
            ConcertArtistRecord(caribou, headliner = false),
          ),
          List(
            TicketRecord(expectedConcert.cheapestTickets(Currency.GBP), Currency.GBP),
            TicketRecord(BigDecimal(99.99), Currency.GBP),
            TicketRecord(BigDecimal(245.0), Currency.GBP),
            TicketRecord(expectedConcert.cheapestTickets(Currency.USD), Currency.USD)
          )
        )
        concert <- concerts.findOne(concertId)
      } yield assert(concert == Some(expectedConcert))
    }.unsafeRunSync()
  }
