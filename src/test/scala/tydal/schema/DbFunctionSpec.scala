package tydal.schema

trait DbFunctionSpec

object AvgSpec extends DbFunctionSpec:
  Avg(Column["x", numeric]): Aggregation[Column["x", numeric], numeric]
  Avg(Column["x", nullable[int2]]): Aggregation[Column["x", nullable[int2]], nullable[float4]]
  Avg(Column["x", nullable[int4]]): Aggregation[Column["x", nullable[int4]], nullable[float4]]
  Avg(Column["x", nullable[int8]]): Aggregation[Column["x", nullable[int8]], nullable[float8]]


object MinSpec extends App with DbFunctionSpec:
  println(Min(Column["x", varchar]))

object UnnestSpec extends DbFunctionSpec:
  Unnest(Column["x", array[varchar]]): DbFunction1[Column["x", array[varchar]], varchar]
