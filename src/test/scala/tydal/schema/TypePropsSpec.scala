package tydal.schema

import scala.util.NotGiven

trait TypePropsSpec


object AreComparableSpec extends TypePropsSpec:
  implicitly[AreComparable[varchar, varchar]]
  implicitly[AreComparable[char, varchar]]
  implicitly[NotGiven[AreComparable[integer, varchar]]]
  implicitly[AreComparable[bool, bool]]
  implicitly[AreComparable[nullable[bool], bool]]
  implicitly[AreComparable[bool, nullable[bool]]]
  implicitly[AreComparable[nullable[bool], nullable[bool]]]

  implicitly[AreComparable[Field[varchar], Field[varchar]]]
  implicitly[AreComparable[Field[char], Field[varchar]]]
  implicitly[NotGiven[AreComparable[Field[integer], Field[varchar]]]]
  implicitly[AreComparable[Field[bool], Field[bool]]]
  implicitly[AreComparable[Field[nullable[bool]], Field[bool]]]
  implicitly[AreComparable[Field[bool], Field[nullable[bool]]]]
  implicitly[AreComparable[Field[nullable[bool]], Field[nullable[bool]]]]


object IsIntegerSpec extends TypePropsSpec:
  implicitly[IsInteger[smallint]]
  implicitly[IsInteger[integer]]
  implicitly[IsInteger[bigint]]
  implicitly[IsInteger[nullable[bigint]]]
  implicitly[IsInteger[Column["some_int", nullable[bigint]]]]
  implicitly[NotGiven[IsInteger[Column["some_float", float4]]]]


object IsRationalSpec extends TypePropsSpec:
  implicitly[IsRational[float4]]
  implicitly[IsRational[float8]]
  implicitly[IsRational[numeric]]
  implicitly[IsRational[nullable[numeric]]]
  implicitly[IsRational[Column["some_int", nullable[numeric]]]]
  implicitly[NotGiven[IsRational[Column["some_int", bigint]]]]


object IsTextSpec extends TypePropsSpec:
  implicitly[IsText[varchar]]
  implicitly[IsText[char]]
  implicitly[IsText[text]]
  implicitly[IsText[nullable[text]]]
  implicitly[IsText[Column["some_text", nullable[text]]]]
  implicitly[NotGiven[IsText[Column["some_float", float4]]]]


object IsNumericalSpec extends TypePropsSpec:
  implicitly[IsNumerical[Column["some_int", nullable[bigint]]]]
  implicitly[IsNumerical[Column["some_float", nullable[float4]]]]
  implicitly[NotGiven[IsNumerical[Column["some_float", nullable[varchar]]]]]


object IsTemporalSpec extends TypePropsSpec:
  implicitly[IsTemporal[timestamp]]
  implicitly[IsTemporal[date]]
  implicitly[IsTemporal[nullable[date]]]
  implicitly[IsTemporal[Column["some_date", nullable[date]]]]
  implicitly[NotGiven[IsTemporal[Column["some_float", float4]]]]


object IsNullSpec extends TypePropsSpec:
  implicitly[IsNullable[nullable[bool]]]
  implicitly[IsNullable[Field[nullable[bool]]]]
  implicitly[NotGiven[IsNullable[bool]]]
  implicitly[NotGiven[IsNullable[Field[bool]]]]


object NullableSpec extends TypePropsSpec:
  val softCast: SoftCast[Column["test", integer], nullable[integer]] = Nullable(Column["test", integer])
  val identity: Column["test", nullable[integer]] = Nullable(Column["test", nullable[integer]])
