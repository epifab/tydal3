package tydal.schema

object EncoderFactorySpec:
  val enc: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = EncoderFactory(Placeholder["x", varchar])
  val _: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = enc
  EncoderFactory("3"[varchar]).encode(EmptyTuple)
  EncoderFactory(("3"[varchar], 4[int4])).encode(EmptyTuple)
  private val value = EncoderFactory((Placeholder["hello", int4], Placeholder["world", varchar]))
  // again buggy
  val xxx: skunk.Encoder[("hello" ~~> Int, "world" ~~> String)] = value
  value.encode((("hello" ~~> 32), ("world" ~~> "foo")))
  EncoderFactory("3"[varchar] *: EmptyTuple).encode(EmptyTuple)
