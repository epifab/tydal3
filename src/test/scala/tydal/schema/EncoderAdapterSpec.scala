package tydal.schema

object EncoderAdapterSpec:
  val enc: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = EncoderAdapter("x".placeholder[varchar])
  val _: skunk.Encoder[(KeyValue["x", String]) *: EmptyTuple] = enc
  EncoderAdapter("3".literal[varchar]).encode(EmptyTuple)
  EncoderAdapter(("3".literal[varchar], 4.literal[int4])).encode(EmptyTuple)
  private val value = EncoderAdapter(("hello".placeholder[int4], "world".placeholder[varchar]))
  // again buggy
  val xxx: skunk.Encoder[("hello" ~~> Int, "world" ~~> String)] = value
  value.encode((("hello" ~~> 32), ("world" ~~> "foo")))
  EncoderAdapter("3".literal[varchar] *: EmptyTuple).encode(EmptyTuple)
