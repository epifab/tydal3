package tydal

object Schema:

  enum Currency:
    case EUR, GBP, USD

  object Currency:
    given Enumerated[Currency] with
      override def fromString(s: String): Option[Currency] = scala.util.Try(Currency.valueOf(s)).toOption

      override def toString(t: Currency): String = t.toString

  enum Genre:
    case Rock, Psychedelic, Electronic, Pop, Metal

  object Genre:
    given Enumerated[Genre] with
      override def fromString(s: String): Option[Genre] = scala.util.Try(Genre.valueOf(s)).toOption

      override def toString(t: Genre): String = t.toString


  object artist extends TableSchema[
    "artist",
    (
      "id" :=: uuid,
      "name" :=: varcharOf[128],
      "genres" :=: array[`enum`["genre", Genre]]
    )
  ]

  object venue extends TableSchema[
    "venue",
    (
      "id" :=: uuid,
      "name" :=: varcharOf[128],
      "address" :=: nullable[text]
    )
  ]

  object concert extends TableSchema[
    "concert",
    (
      "id" :=: uuid,
      "venue_id" :=: uuid,
      "begins_at" :=: timestamp,
      "ends_at" :=: timestamp
    )
  ]

  object ticket extends TableSchema[
    "ticket",
    (
      "id" :=: uuid,
      "concert_id" :=: uuid,
      "price" :=: numeric,
      "currency" :=: `enum`["currency", Currency]
    )
  ]

  object concert_artist extends TableSchema[
    "concert_artist",
    (
      "concert_id" :=: uuid,
      "artist_id" :=: uuid,
      "headliner" :=: bool,
      "index" :=: int4
    )
  ]
