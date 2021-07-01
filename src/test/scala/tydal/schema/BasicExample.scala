package tydal.schema

import cats.effect.IO
import skunk.data.Arr
import skunk.{Query, Session}
import tydal.schema._

object artist extends TableSchema[
  "artist",
  (
    "id" :=: uuid,
    "name" :=: varcharOf[128],
    "genres" :=: array[varchar]
  )
]

val query =
  Select
    .from(artist as "a")
    .take(x => (x("a", "id"), x("a", "name"), x("a", "genres")))
    .where(x => (x("a", "genres") overlaps "genres?") or (x("a", "name") like "name?"))
    .sortBy(_("a", "name"))
    .inRange("offset?", "limit?")
    .compile

// For more information on how to initiate a skunk Session please refer to https://tpolecat.github.io/skunk/
def runQuery(session: Session[IO]): IO[List[(java.util.UUID, String, Arr[String])]] =
  session.prepare(query).use(_.stream((
    "genres?" ~~> Arr("Rock", "Psychedelic"),
    "name?" ~~> "%Floyd",
    "offset?" ~~> 0L,
    "limit?" ~~> 50
  ), 8).compile.toList)
