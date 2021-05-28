package tydal.schema

trait DbFunctionSpec

object AvgSpec extends DbFunctionSpec:
  Avg(Column["x", numeric]): DbAggregationFunction[Column["x", numeric], numeric]
  Avg(Column["x", nullable[smallint]]): DbAggregationFunction[Column["x", nullable[smallint]], nullable[float4]]
  Avg(Column["x", nullable[integer]]): DbAggregationFunction[Column["x", nullable[integer]], nullable[float4]]
  Avg(Column["x", nullable[bigint]]): DbAggregationFunction[Column["x", nullable[bigint]], nullable[float8]]


object UnnestSpec extends DbFunctionSpec:
  Unnest(Column["x", array[bool]]): DbFunction1[Column["x", array[bool]], bool]
