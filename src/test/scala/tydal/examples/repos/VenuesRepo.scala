package tydal.examples.repos

import cats.Monad
import cats.Monad.ops.toAllMonadOps
import cats.effect.Resource
import skunk.Session
import tydal._
import Schema.{Genre, venue}

import java.util.UUID

trait VenuesRepo[F[_]]:
  def add(name: String, address: Option[String], geoLocation: Option[(Double, Double)]): F[UUID]

object VenuesRepo:
  private val insertCommand =
    Insert
      .into(venue)
      .fields(v => (
        v("id"),
        v("name"),
        v("address"),
        v("geo_location") :== Placeholder["geo_location", nullable[postgis.point]]
          .castTo[postgis.geometry]
          .castTo[postgis.geography]
      ))
      .compile

  def apply[F[_]: Monad](newId: F[UUID], session: Resource[F, Session[F]]): Resource[F, VenuesRepo[F]] =
    for {
      s <- session
      insertStatement <- s.prepare(insertCommand)
      repo = new VenuesRepo[F]:
        def add(name: String, address: Option[String], geoLocation: Option[(Double, Double)]): F[UUID] =
          for {
            id <- newId
            _ <- insertStatement.execute((
              "id" ~~> id,
              "name" ~~> name,
              "address" ~~> address,
              "geo_location" ~~> geoLocation
            ))
          } yield id
    } yield repo
