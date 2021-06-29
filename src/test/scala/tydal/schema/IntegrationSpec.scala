package tydal.schema

import cats.effect._
import cats.Monad
import cats.implicits._
import natchez.Trace.Implicits.noop
import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk._
import skunk.codec.all._
import skunk.data.Arr
import skunk.implicits._
import tydal.schema.repos.Schema._
import tydal.schema.repos._

import java.time.Instant
import java.time.LocalDate
import java.util.UUID


object IntegrationSpec extends IOApp:
  
  val session: Resource[IO, Session[IO]] =
    Session.single(
      host     = "localhost",
      port     = 5432,
      user     = "root",
      password = Some("p4ssw0rd"),
      database = "tydal",
      strategy = Strategy.SearchPath
    )

  def run(args: List[String]): IO[ExitCode] =
    val repos = (for {
      artistsRepo <- ArtistsRepo(IO(UUID.randomUUID()), session)
      venuesRepo <- VenuesRepo(IO(UUID.randomUUID()), session)
      concertsRepo <- ConcertsRepo(IO(UUID.randomUUID()), session)
    } yield ((artistsRepo, venuesRepo, concertsRepo)))

    repos.use { case (artists, venues, concerts) =>
      for {
        radiohead <- artists.create("Radiohead", List(Genre.Rock, Genre.Electronic, Genre.Psychedelic))
        caribou <- artists.create("Caribou", List(Genre.Electronic))
        roundhouse <- venues.create("Roundhouse", Some("London"))
        concertId <- concerts.create(
          roundhouse,
          Instant.parse("2015-03-01T20:00:00Z"),
          Instant.parse("2015-03-02T01:00:00Z"),
          List(
            ConcertArtistRecord(radiohead, headliner = true),
            ConcertArtistRecord(caribou, headliner = false),
          ),
          List(
            TicketRecord(BigDecimal(15.5), Currency.GBP),
            TicketRecord(BigDecimal(45.0), Currency.GBP),
            TicketRecord(BigDecimal(90.0), Currency.USD)
          )
        )
        concert <- concerts.findOne(concertId)
        _ <- IO(println(concert))
      } yield ExitCode.Success
    }

