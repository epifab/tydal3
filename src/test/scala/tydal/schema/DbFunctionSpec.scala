package tydal.schema

trait DbFunctionSpec

object AvgSpec extends DbFunctionSpec:
  Avg(Column["x", numeric]): Aggregation[Column["x", numeric], numeric]
  Avg(Column["x", nullable[smallint]]): Aggregation[Column["x", nullable[smallint]], nullable[float4]]
  Avg(Column["x", nullable[integer]]): Aggregation[Column["x", nullable[integer]], nullable[float4]]
  Avg(Column["x", nullable[bigint]]): Aggregation[Column["x", nullable[bigint]], nullable[float8]]


object MinSpec extends App with DbFunctionSpec:
  println(Min(Column["x", varchar]))

object UnnestSpec extends DbFunctionSpec:
  Unnest(Column["x", array[bool]]): DbFunction1[Column["x", array[bool]], bool]
