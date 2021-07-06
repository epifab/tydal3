package tydal.test

import skunk.{Decoder, Void}
import tydal._
import tydal.compiler.DecoderFactory

object DecoderFactorySpec:
  DecoderFactory("3"[varchar]): Decoder[String]
  DecoderFactory(("3"[varchar], 4[int4])): Decoder[(String, Int)]
  DecoderFactory(Column["hello", nullable[int4]]): Decoder[Option[Int]]
  DecoderFactory(Add(4[int4], 5[int4])): Decoder[Int]
