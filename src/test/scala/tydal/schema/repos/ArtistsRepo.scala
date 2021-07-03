package tydal.schema.repos

import cats.Monad
import cats.Monad.ops.toAllMonadOps
import cats.effect.Resource
import skunk.Session
import tydal.schema._
import tydal.schema.repos.Schema._

import java.util.UUID


trait ArtistsRepo[F[_]]:
  def create(name: String, genres: List[Genre]): F[UUID]

object ArtistsRepo:
  private val insertCommand =
    Insert
      .into(artist)
      .fields(a => (
        a("id"),
        a("name"),
        a("genres")
      ))
      .compile

  def apply[F[_]: Monad](newId: F[UUID], session: Resource[F, Session[F]]): Resource[F, ArtistsRepo[F]] =
    for {
      s <- session
      insertStatement <- s.prepare(insertCommand)
      repo = new ArtistsRepo[F]:
        def create(name: String, genres: List[Genre]): F[UUID] =
          for {
            id <- newId
            _ <- insertStatement.execute((
              "id" ~~> id,
              "name" ~~> name,
              "genres" ~~> skunk.data.Arr(genres: _*)
            ))
          } yield id
    } yield repo
