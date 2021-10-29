package tydal.test

import skunk.{Encoder, Void}
import tydal.*
import tydal.compiler.EncoderFactory

import java.util.UUID

object EncoderFactorySpec:
  EncoderFactory("3"[varchar]): Encoder[Void]
  EncoderFactory(("3"[varchar], 4[int4])): Encoder[Void]

  EncoderFactory(Placeholder["x", varchar])
    .encode("x" ~~> "foo")

  EncoderFactory((Placeholder["hello", int4], Placeholder["world", varchar]))
    .encode(("hello" ~~> 3, "world" ~~> "foo"))

  EncoderFactory(("3"[varchar], Placeholder["hello", int4], 4[int4], Placeholder["world", varchar]))
    .encode((("hello" ~~> 3, "world" ~~> "foo")))

  EncoderFactory(("3"[varchar], "3"[varchar], Placeholder["hello", int4], "3"[varchar], Placeholder["world", varchar]))
    .encode(("hello" ~~> 4, "world" ~~> "foo"))

  Update(Schema.ticket)
    .set(t => t("price") :== 19.8[float8])
    .where(_("concert_id") === "id?")
    .compile: skunk.Command["id?" ~~> UUID]
