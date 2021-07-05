package tydal.test

import cats.effect.IO
import skunk.data.Arr
import skunk.{Query, Session}
import tydal._

object Schema:
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
    .from(Schema.artist as "a")
    .take(_("a").*)
    .where(x => (x("a", "genres") overlaps "genres?") or (x("a", "name") like "name?"))
    .sortBy(_("a", "name"))
    .offset(0L[int8])
    .limit("limit?")
    .compile

// For more information on how to initiate a Session
// or to run a query please refer to https://tpolecat.github.io/skunk/
def runQuery(session: Session[IO]): IO[List[(java.util.UUID, String, Arr[String])]] =
  session.prepare(query).use(_.stream((
    "genres?" ~~> Arr("Rock", "Psychedelic"),
    "name?" ~~> "%Floyd",
    "limit?" ~~> 50
  ), 10).compile.toList)
