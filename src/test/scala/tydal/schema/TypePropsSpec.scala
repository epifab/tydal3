package tydal.schema

import scala.util.NotGiven

trait TypePropsSpec


object AreComparableSpec extends TypePropsSpec:
  summon[AreComparable[varchar, varchar]]
  summon[AreComparable[char, varchar]]
  summon[NotGiven[AreComparable[integer, varchar]]]
  summon[AreComparable[bool, bool]]
  summon[AreComparable[nullable[bool], bool]]
  summon[AreComparable[bool, nullable[bool]]]
  summon[AreComparable[nullable[bool], nullable[bool]]]
  summon[AreComparable[nullable[char], varchar]]
  summon[AreComparable[char, nullable[varchar]]]
  summon[AreComparable[nullable[char], nullable[varchar]]]

  summon[AreComparable[Placeholder[varchar], Column["hello", varchar]]]
  summon[AreComparable[Placeholder[char], Column["hello", varchar]]]
  summon[NotGiven[AreComparable[Field[integer], Field[varchar]]]]
  summon[AreComparable[Field[bool], Field[bool]]]
  summon[AreComparable[Column["yo", nullable[bool]], Placeholder[bool]]]
  summon[AreComparable[Column["yo", bool], Placeholder[nullable[bool]]]]
  summon[AreComparable[Column["yo", nullable[bool]], Placeholder[nullable[bool]]]]
  summon[AreComparable[Column["yo", nullable[char]], Placeholder[varchar]]]
  summon[AreComparable[Column["yo", char], Placeholder[nullable[varchar]]]               ]
  summon[AreComparable[Column["yo", nullable[char]], Placeholder[nullable[varchar]]]]


object AreComparableArraySpec extends TypePropsSpec:
  summon[AreComparableArray[array[varchar], array[varchar]]]
  summon[AreComparableArray[array[char], array[varchar]]]
  summon[NotGiven[AreComparableArray[array[integer], array[varchar]]]]
  summon[AreComparableArray[array[bool], array[bool]]]
  summon[AreComparableArray[nullable[array[bool]], array[bool]]]
  summon[AreComparableArray[array[bool], nullable[array[bool]]]]
  summon[AreComparableArray[nullable[array[bool]], nullable[array[bool]]]]
  summon[AreComparableArray[nullable[array[char]], array[varchar]]]
  summon[AreComparableArray[array[char], nullable[array[varchar]]]]
  summon[AreComparableArray[nullable[array[char]], nullable[array[varchar]]]]

  summon[AreComparableArray[Placeholder[array[varchar]], Column["hello", array[varchar]]]]
  summon[AreComparableArray[Placeholder[array[char]], Column["hello", array[varchar]]]]
  summon[NotGiven[AreComparableArray[Field[array[integer]], Field[array[varchar]]]]]
  summon[AreComparableArray[Field[array[bool]], Field[array[bool]]]]
  summon[AreComparableArray[Column["yo", nullable[array[bool]]], Placeholder[array[bool]]]]
  summon[AreComparableArray[Column["yo", array[bool]], Placeholder[nullable[array[bool]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[bool]]], Placeholder[nullable[array[bool]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[char]]], Placeholder[array[varchar]]]]
  summon[AreComparableArray[Column["yo", array[char]], Placeholder[nullable[array[varchar]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[char]]], Placeholder[nullable[array[varchar]]]]]


object CanContainSpec extends TypePropsSpec:
  summon[CanContain[array[varchar], varchar]]
  summon[CanContain[array[char], varchar]]
  summon[NotGiven[CanContain[array[integer], varchar]]]
  summon[CanContain[array[bool], bool]]
  summon[CanContain[nullable[array[bool]], bool]]
  summon[CanContain[array[bool], nullable[bool]]]
  summon[CanContain[nullable[array[bool]], nullable[bool]]]
  summon[CanContain[nullable[array[char]], varchar]]
  summon[CanContain[array[char], nullable[varchar]]]
  summon[CanContain[nullable[array[char]], nullable[varchar]]]

  summon[CanContain[Placeholder[array[varchar]], Column["hello", varchar]]]
  summon[CanContain[Placeholder[array[char]], Column["hello", varchar]]]
  summon[NotGiven[CanContain[Field[array[integer]], Field[varchar]]]]
  summon[CanContain[Field[array[bool]], Field[bool]]]
  summon[CanContain[Column["yo", nullable[array[bool]]], Placeholder[bool]]]
  summon[CanContain[Column["yo", array[bool]], Placeholder[nullable[bool]]]]
  summon[CanContain[Column["yo", nullable[array[bool]]], Placeholder[nullable[bool]]]]
  summon[CanContain[Column["yo", nullable[array[char]]], Placeholder[varchar]]]
  summon[CanContain[Column["yo", array[char]], Placeholder[nullable[varchar]]]]
  summon[CanContain[Column["yo", nullable[array[char]]], Placeholder[nullable[varchar]]]]

object IsIntegerSpec extends TypePropsSpec:
  summon[IsInteger[smallint]]
  summon[IsInteger[integer]]
  summon[IsInteger[bigint]]
  summon[IsInteger[nullable[bigint]]]
  summon[IsInteger[Column["some_int", nullable[bigint]]]]
  summon[NotGiven[IsInteger[Column["some_float", float4]]]]


object IsRationalSpec extends TypePropsSpec:
  summon[IsRational[float4]]
  summon[IsRational[float8]]
  summon[IsRational[numeric]]
  summon[IsRational[nullable[numeric]]]
  summon[IsRational[Column["some_int", nullable[numeric]]]]
  summon[NotGiven[IsRational[Column["some_int", bigint]]]]


object IsTextSpec extends TypePropsSpec:
  summon[IsText[varchar]]
  summon[IsText[char]]
  summon[IsText[text]]
  summon[IsText[nullable[text]]]
  summon[IsText[Column["some_text", nullable[text]]]]
  summon[NotGiven[IsText[Column["some_float", float4]]]]


object IsNumericalSpec extends TypePropsSpec:
  summon[IsNumerical[Column["some_int", nullable[bigint]]]]
  summon[IsNumerical[Column["some_float", nullable[float4]]]]
  summon[NotGiven[IsNumerical[Column["some_float", nullable[varchar]]]]]


object IsTemporalSpec extends TypePropsSpec:
  summon[IsTemporal[timestamp]]
  summon[IsTemporal[date]]
  summon[IsTemporal[nullable[date]]]
  summon[IsTemporal[Column["some_date", nullable[date]]]]
  summon[IsTemporal[Field[nullable[date]]]]
  summon[NotGiven[IsTemporal[Column["some_float", float4]]]]
  summon[NotGiven[IsTemporal[Field[float4]]]]


object IsNullableSpec extends TypePropsSpec:
  summon[IsNullable[nullable[bool]]]
  summon[IsNullable[Field[nullable[bool]]]]
  summon[NotGiven[IsNullable[bool]]]
  summon[NotGiven[IsNullable[Field[bool]]]]


object NullableSpec extends TypePropsSpec:
  val softCast: SoftCast[Column["test", integer], nullable[integer]] = Nullable(Column["test", integer])
  val identity: Column["test", nullable[integer]] = Nullable(Column["test", nullable[integer]])
