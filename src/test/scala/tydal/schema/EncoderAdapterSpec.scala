package tydal.schema

object EncoderAdapterSpec:
  val enc: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = EncoderAdapter(Placeholder["x", varchar])
  val _: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = enc
  EncoderAdapter("3"[varchar]).encode(EmptyTuple)
  EncoderAdapter(("3"[varchar], 4[int4])).encode(EmptyTuple)
  private val value = EncoderAdapter((Placeholder["hello", int4], Placeholder["world", varchar]))
  // again buggy
  val xxx: skunk.Encoder[("hello" ~~> Int, "world" ~~> String)] = value
  value.encode((("hello" ~~> 32), ("world" ~~> "foo")))
  EncoderAdapter("3"[varchar] *: EmptyTuple).encode(EmptyTuple)
