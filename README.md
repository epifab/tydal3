# Tydal 3

[![Join the chat at https://gitter.im/tydal3/community](https://badges.gitter.im/tydal3/community.svg)](https://gitter.im/tydal3/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Tydal 3 is a typesafe PostgreSQL DSL for *Scala 3* built on top of [Skunk](https://github.com/tpolecat/skunk).


## Why Tydal 3

Tydal 3 is essentially a DSL to build Postgres queries and commands
that could be used instead of the classic Skunk `sql` and `fr` String interpolation.

The main advantage of going through the Tydal DSL is type-safety,
as a bunch of common mistakes can be prevented at compile time.  
For instance, it won't be possible to refer to a non-existing table or field,
comparing fields of an unrelated type,
or decoding the results of a query into an incompatible data structure.

Here's a minimal example:

```scala
import tydal.schema._
import skunk.Query
import skunk.data.Arr

object artist extends TableSchema[
  "artist",
  (
    "id" :=: uuid,
    "name" :=: varcharOf[128],
    "genres" :=: array[varchar]
  )
]

val query: Query[
  (
    "genres?" ~~> Arr[String], 
    "name?" ~~> String
  ),
  (
    java.util.UUID,
    String,
    Arr[String]
  )
] = 
  Select
    .from(artist as "a")
    .where(x => (x("a", "genres") intersects "genres?") or (x("a", "name") like "name?"))
    .sortBy(_("a", "name"))
    .compile
```

## Support

Tydal supports a fair amount of SQL syntax, including *joins*, *subqueries*, *aggregation functions* and more,
although it's obviously not exhaustive.  
The project is open source and everyone is welcome to collaborate!


## Release

Not there yet! Stay tuned.
