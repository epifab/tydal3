package tydal.schema

object EncoderAdapterSpec:
  EncoderAdapter("3".literal[varchar]).encode(EmptyTuple)
  EncoderAdapter(("3".literal[varchar], 4.literal[integer])).encode(EmptyTuple)
  EncoderAdapter(("hello".placeholder[integer], "world".placeholder[varchar])).encode((("hello" ~~> 32), ("world" ~~> "foo")))
  EncoderAdapter("3".literal[varchar] *: EmptyTuple).encode(EmptyTuple)
