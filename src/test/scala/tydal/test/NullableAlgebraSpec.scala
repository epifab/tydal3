package tydal
package test

import tydal.utils.{AnyNullableAlgebra, EveryNullableAlgebra}

object NullableAlgebraSpec:

  class TypeInferenceTest[X]:
    def is[Y](using Y =:= X): Unit = ()

  class $[A, B]:
    def everyNullableAlgebra[X](using EveryNullableAlgebra[A, B, X]): TypeInferenceTest[X] = new TypeInferenceTest[X]
    def anyNullableAlgebra[X](using AnyNullableAlgebra[A, B, X]): TypeInferenceTest[X] = new TypeInferenceTest[X]

  $[varchar, int4].everyNullableAlgebra.is[int4]
  $[nullable[varchar], int4].everyNullableAlgebra.is[nullable[int4]]
  $[EmptyTuple, int4].everyNullableAlgebra.is[nullable[int4]]
  $[(float8, int8), int4].everyNullableAlgebra.is[int4]
  $[(float8, nullable[int8]), int4].everyNullableAlgebra.is[int4]
  $[(nullable[float8], int8), int4].everyNullableAlgebra.is[int4]
  $[(nullable[float8], nullable[int8]), int4].everyNullableAlgebra.is[nullable[int4]]

  $[varchar, int4].anyNullableAlgebra.is[int4]
  $[nullable[varchar], int4].anyNullableAlgebra.is[nullable[int4]]
  $[EmptyTuple, int4].anyNullableAlgebra.is[int4]
  $[(float8, int8), int4].anyNullableAlgebra.is[int4]
  $[(float8, nullable[int8]), int4].anyNullableAlgebra.is[nullable[int4]]
  $[(nullable[float8], int8), int4].anyNullableAlgebra.is[nullable[int4]]
  $[(nullable[float8], nullable[int8]), int4].anyNullableAlgebra.is[nullable[int4]]
