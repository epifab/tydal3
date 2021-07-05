# Tydal 3

[![Join the chat at https://gitter.im/tydal3/community](https://badges.gitter.im/tydal3/community.svg)](https://gitter.im/tydal3/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Tydal 3 is a typesafe PostgreSQL DSL for *Scala 3* built on top of [Skunk](https://github.com/tpolecat/skunk).  
If you're looking for a Scala 2 compatible version of this library, please refer to [Tydal](https://github.com/epifab/tydal).


## Why Tydal 3

Tydal 3 is essentially a DSL to build Postgres queries and commands
that could be used instead of the classic Skunk `sql` and `fr` String interpolations.

The main advantage of going through the Tydal DSL is type-safety,
as a bunch of common mistakes can be prevented at compile time such as 
referring to a non-existing table or field,
comparing fields of unrelated types,
or decoding the results of a query into an incompatible data structure.

Here's a basic example:
```scala
import cats.effect.IO
import skunk.data.Arr
import skunk.{Query, Session}
import tydal._

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
    .take(_("a").*)
    .where(x => (x("a", "genres") overlaps "genres?") or (x("a", "name") like "name?"))
    .sortBy(_("a", "name"))
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
```

## Support

Tydal supports a fair amount of SQL syntax, including *joins*, *subqueries*, *aggregation functions* and more,
although it's obviously not exhaustive.  
The project is open source and you're welcome to collaborate.


## Release

Not there yet! Stay tuned.
