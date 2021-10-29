package tydal.examples

import cats.Monad
import cats.effect.*
import cats.effect.unsafe.IORuntime
import cats.implicits.*
import org.scalatest.freespec.*
import org.scalatest.matchers.*
import skunk.*
import skunk.codec.all.*
import skunk.data.Arr
import skunk.implicits.*
import tydal.Schema.{Currency, Genre}
import tydal.examples.repos.*
import tydal.test.IntegrationTesting

import java.time.{Instant, LocalDate}
import java.util.UUID


class RepositoryExample extends AnyFreeSpec with should.Matchers with IntegrationTesting:

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
        radiohead <- artists.add("Radiohead", List(Genre.Rock, Genre.Electronic, Genre.Psychedelic))
        caribou <- artists.add("Caribou", List(Genre.Electronic))
        roundhouse <- venues.add(expectedConcert.venueName, Some("London"), Some((51.5424717,-0.1472787)))
        concertId <- concerts.add(
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
