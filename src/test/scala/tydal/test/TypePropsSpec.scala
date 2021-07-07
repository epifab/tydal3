package tydal.test

import tydal._

import scala.util.NotGiven

trait TypePropsSpec


object AreComparableSpec extends TypePropsSpec:
  summon[AreComparable[varchar, varchar]]
  summon[AreComparable[text, varchar]]
  summon[NotGiven[AreComparable[int4, varchar]]]
  summon[AreComparable[bool, bool]]
  summon[AreComparable[nullable[bool], bool]]
  summon[AreComparable[bool, nullable[bool]]]
  summon[AreComparable[nullable[bool], nullable[bool]]]
  summon[AreComparable[nullable[text], varchar]]
  summon[AreComparable[text, nullable[varchar]]]
  summon[AreComparable[nullable[text], nullable[varchar]]]

  summon[AreComparable[Const[varchar], Column["hello", varchar]]]
  summon[AreComparable[Const[text], Column["hello", varchar]]]
  summon[NotGiven[AreComparable[Field[int4], Field[varchar]]]]
  summon[AreComparable[Field[bool], Field[bool]]]
  summon[AreComparable[Column["yo", nullable[bool]], Const[bool]]]
  summon[AreComparable[Column["yo", bool], Const[nullable[bool]]]]
  summon[AreComparable[Column["yo", nullable[bool]], Const[nullable[bool]]]]
  summon[AreComparable[Column["yo", nullable[text]], Const[varchar]]]
  summon[AreComparable[Column["yo", text], Const[nullable[varchar]]]               ]
  summon[AreComparable[Column["yo", nullable[text]], Const[nullable[varchar]]]]


object AreComparableArraySpec extends TypePropsSpec:
  summon[AreComparableArray[array[varchar], array[varchar]]]
  summon[AreComparableArray[array[text], array[varchar]]]
  summon[NotGiven[AreComparableArray[array[int4], array[varchar]]]]
  summon[AreComparableArray[array[bool], array[bool]]]
  summon[AreComparableArray[nullable[array[bool]], array[bool]]]
  summon[AreComparableArray[array[bool], nullable[array[bool]]]]
  summon[AreComparableArray[nullable[array[bool]], nullable[array[bool]]]]
  summon[AreComparableArray[nullable[array[text]], array[varchar]]]
  summon[AreComparableArray[array[text], nullable[array[varchar]]]]
  summon[AreComparableArray[nullable[array[text]], nullable[array[varchar]]]]

  summon[AreComparableArray[Const[array[varchar]], Column["hello", array[varchar]]]]
  summon[AreComparableArray[Const[array[text]], Column["hello", array[varchar]]]]
  summon[NotGiven[AreComparableArray[Field[array[int4]], Field[array[varchar]]]]]
  summon[AreComparableArray[Field[array[bool]], Field[array[bool]]]]
  summon[AreComparableArray[Column["yo", nullable[array[bool]]], Const[array[bool]]]]
  summon[AreComparableArray[Column["yo", array[bool]], Const[nullable[array[bool]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[bool]]], Const[nullable[array[bool]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[text]]], Const[array[varchar]]]]
  summon[AreComparableArray[Column["yo", array[text]], Const[nullable[array[varchar]]]]]
  summon[AreComparableArray[Column["yo", nullable[array[text]]], Const[nullable[array[varchar]]]]]


object CanContainSpec extends TypePropsSpec:
  summon[CanContain[array[varchar], varchar]]
  summon[CanContain[array[text], varchar]]
  summon[NotGiven[CanContain[array[int4], varchar]]]
  summon[CanContain[array[bool], bool]]
  summon[CanContain[nullable[array[bool]], bool]]
  summon[CanContain[array[bool], nullable[bool]]]
  summon[CanContain[nullable[array[bool]], nullable[bool]]]
  summon[CanContain[nullable[array[text]], varchar]]
  summon[CanContain[array[text], nullable[varchar]]]
  summon[CanContain[nullable[array[text]], nullable[varchar]]]

  summon[CanContain[Const[array[varchar]], Column["hello", varchar]]]
  summon[CanContain[Const[array[text]], Column["hello", varchar]]]
  summon[NotGiven[CanContain[Field[array[int4]], Field[varchar]]]]
  summon[CanContain[Field[array[bool]], Field[bool]]]
  summon[CanContain[Column["yo", nullable[array[bool]]], Const[bool]]]
  summon[CanContain[Column["yo", array[bool]], Const[nullable[bool]]]]
  summon[CanContain[Column["yo", nullable[array[bool]]], Const[nullable[bool]]]]
  summon[CanContain[Column["yo", nullable[array[text]]], Const[varchar]]]
  summon[CanContain[Column["yo", array[text]], Const[nullable[varchar]]]]
  summon[CanContain[Column["yo", nullable[array[text]]], Const[nullable[varchar]]]]

object IsIntegerSpec extends TypePropsSpec:
  summon[IsInteger[int2]]
  summon[IsInteger[int4]]
  summon[IsInteger[int8]]
  summon[IsInteger[nullable[int8]]]
  summon[IsInteger[Column["some_int", nullable[int8]]]]
  summon[NotGiven[IsInteger[Column["some_float", float4]]]]


object IsRationalSpec extends TypePropsSpec:
  summon[IsRational[float4]]
  summon[IsRational[float8]]
  summon[IsRational[numeric]]
  summon[IsRational[nullable[numeric]]]
  summon[IsRational[Column["some_int", nullable[numeric]]]]
  summon[NotGiven[IsRational[Column["some_int", int8]]]]


object IsTextSpec extends TypePropsSpec:
  summon[IsText[varchar]]
  summon[IsText[varcharOf[99]]]
  summon[IsText[text]]
  summon[IsText[nullable[text]]]
  summon[IsText[Column["some_text", nullable[text]]]]
  summon[NotGiven[IsText[Column["some_float", float4]]]]


object IsNumericalSpec extends TypePropsSpec:
  summon[IsNumerical[Column["some_int", nullable[int8]]]]
  summon[IsNumerical[Column["some_float", nullable[float4]]]]
  summon[IsNumerical[Column["some_numeric", nullable[numericOf[10, 4]]]]]
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
  val softCast: SoftCast[Column["test", int4], nullable[int4]] = Nullable(Column["test", int4])
  val identity: Column["test", nullable[int4]] = Nullable(Column["test", nullable[int4]])
