package tydal.test

import tydal._

trait DbFunctionSpec

object AvgSpec extends DbFunctionSpec:
  Avg(Column["x", int2]): Aggregation[Column["x", int2], nullable[numeric]]
  Avg(Column["x", int4]): Aggregation[Column["x", int4], nullable[numeric]]
  Avg(Column["x", int8]): Aggregation[Column["x", int8], nullable[numeric]]
  Avg(Column["x", float4]): Aggregation[Column["x", float4], nullable[float8]]
  Avg(Column["x", float8]): Aggregation[Column["x", float8], nullable[float8]]
  Avg(Column["x", numeric]): Aggregation[Column["x", numeric], nullable[numeric]]
  Avg(Column["x", nullable[int2]]): Aggregation[Column["x", nullable[int2]], nullable[numeric]]
  Avg(Column["x", nullable[int4]]): Aggregation[Column["x", nullable[int4]], nullable[numeric]]
  Avg(Column["x", nullable[int8]]): Aggregation[Column["x", nullable[int8]], nullable[numeric]]
  Avg(Column["x", nullable[float4]]): Aggregation[Column["x", nullable[float4]], nullable[float8]]
  Avg(Column["x", nullable[float8]]): Aggregation[Column["x", nullable[float8]], nullable[float8]]
  Avg(Column["x", nullable[numeric]]): Aggregation[Column["x", nullable[numeric]], nullable[numeric]]

object UnnestSpec extends DbFunctionSpec:
  Unnest(Column["x", array[varchar]]): DbFunction1[Column["x", array[varchar]], varchar]
