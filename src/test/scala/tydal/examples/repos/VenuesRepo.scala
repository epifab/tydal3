package tydal.examples.repos

import cats.Monad
import cats.Monad.ops.toAllMonadOps
import cats.effect.Resource
import skunk.Session
import tydal._
import Schema.{Genre, venue}

import java.util.UUID

trait VenuesRepo[F[_]]:
  def create(name: String, address: Option[String]): F[UUID]

object VenuesRepo:
  private val insertCommand =
    Insert
      .into(venue)
      .fields(v => (
        v("id"),
        v("name"),
        v("address")
      ))
      .compile

  def apply[F[_]: Monad](newId: F[UUID], session: Resource[F, Session[F]]): Resource[F, VenuesRepo[F]] =
    for {
      s <- session
      insertStatement <- s.prepare(insertCommand)
      repo = new VenuesRepo[F]:
        def create(name: String, address: Option[String]): F[UUID] =
          for {
            id <- newId
            _ <- insertStatement.execute((
              "id" ~~> id,
              "name" ~~> name,
              "address" ~~> address
            ))
          } yield id
    } yield repo
