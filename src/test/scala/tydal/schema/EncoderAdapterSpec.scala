package tydal.schema

object EncoderAdapterSpec:

  val x = EncoderAdapter("3".literal[varchar])
  x.encode(EmptyTuple)
  val y = EncoderAdapter(("3".literal[varchar], 4.literal[integer]))
  y.encode(EmptyTuple)
  // todo: why doesn't this work?
  // val w = EncoderAdapter(("hello".placeholder[integer], "world".placeholder[varchar])).encode((("hello" ~~> 32), ("world" ~~> "foo")))
  // w.encode(("hello" ~~> 32) *: EmptyTuple)
  val z = EncoderAdapter("3".literal[varchar] *: EmptyTuple)
  z.encode(EmptyTuple)


// todo: if I try to fix x, y, z, or w as Decoder[EmptyTuple] it doesn't compile, but in the end that's what it is
//  summon[EncoderAdapter[Literal[varchar], EmptyTuple]]
//  summon[EncoderAdapter[Literal[varchar] *: EmptyTuple, EmptyTuple]]
//  summon[EncoderAdapter[Literal[varchar] *: EmptyTuple, EmptyTuple]](
//    using EncoderAdapter.nonEmpty(
//      using
//      EncoderAdapter.literal[varchar],
//      EncoderAdapter.empty
//    )
//  )
//
//  summon[EncoderAdapter[(Literal[varchar], Literal[integer]), EmptyTuple]](
//    using EncoderAdapter.nonEmpty(
//      using
//      EncoderAdapter.literal[varchar],
//      EncoderAdapter.nonEmpty(
//        using
//        EncoderAdapter.literal[integer],
//        EncoderAdapter.empty
//      )
//    )
//  )
//  summon[EncoderAdapter[Placeholder["yo", varchar], ("yo", String) *: EmptyTuple]]
