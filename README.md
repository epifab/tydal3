# Tydal 3

[![Build and test](https://github.com/epifab/tydal3/actions/workflows/main.yml/badge.svg)](https://github.com/epifab/tydal3/actions/workflows/main.yml)

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
import skunk.{Query, Session}
import tydal.*

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
    .where(x => (x("a", "genres") contains "genre?") and (x("a", "name") like "name?"))
    .sortBy(_("a", "name"))
    .limit("limit?")
    .compile

// For more information on how to initiate a Session
// or to run a query please refer to https://tpolecat.github.io/skunk/
def runQuery(session: Session[IO]): IO[List[(java.util.UUID, String, Arr[String])]] =
  session.prepareR(query).use(_.stream((
    "genre?" ~~> "Psychedelic",
    "name?" ~~> "%Floyd",
    "limit?" ~~> 50
  ), 10).compile.toList)
```

## Support

Tydal supports a fair amount of PostgreSQL, and most common queries can be expressed through the DSL,
but not everything is or can be supported, of course.  
For a complete list of supported features please refer to [this documentation](support.md).  
You can find some examples [here](src/test/scala/tydal/examples).  
The project is open source, and you're very welcome to contribute!

## Testing

```shell
$ docker-compose up -d
$ sbt test
```


## Getting started

### Installation (sbt)

Step 1. Add the JitPack repository to your build file

```
resolvers += "jitpack" at "https://jitpack.io"
```

Step 2. Add the dependency

```
libraryDependencies += "com.github.epifab" % "tydal3" % "0.3"	
```
