package tydal.schema

import skunk.{Encoder, Void}

object EncoderFactorySpec:
  EncoderFactory("3"[varchar]): Encoder[Void]
  EncoderFactory(("3"[varchar], 4[int4])): Encoder[Void]

  EncoderFactory(Placeholder["x", varchar])
    .encode("x" ~~> "foo")

  EncoderFactory((Placeholder["hello", int4], Placeholder["world", varchar]))
    .encode(("hello" ~~> 3, "world" ~~> "foo"))

  EncoderFactory(("3"[varchar], Placeholder["hello", int4], 4[int4], Placeholder["world", varchar]))
    .encode((("hello" ~~> 3, "world" ~~> "foo")))
