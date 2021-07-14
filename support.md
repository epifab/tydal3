# Supported features

## Column types

- `varchar`
- `varcharOf[size]`
- `text`
- `int2`
- `int4`
- `int8`
- `float4`
- `float8`
- `numeric`
- `numericOf[precision,scale]`
- `bool`
- `uuid`
- `date`
- `timestamp`
- `array[varchar]`
- `array[text]`
- `array[int2]`
- `array[int4]`
- `array[int8]`
- `array[float4]`
- `array[float8]`
- `array[numeric]`
- `array[uuid]`
- `enum`
- `json`
- `jsonb`

## Arithmetic functions

- `+`
- `-`
- `*`
- `/`

## String functions

- `Concat`
- `Lower`
- `Upper`
- `CharLength`

## Aggregations

- `Sum`
- `Min`
- `Max`
- `Avg`
- `Count`

## Other expressions

- `Cast`
- `Coalesce`
- `Nullable` (marks a field as nullable)
- `Unnest`

## Logical operators

- `and`
- `or`
- `isDefined`
- `isNotDefined`
- `===`
- `<>`
- `>`
- `>=`
- `<`
- `<=`
- `like`
- `ilike`
- `@>` (`supersetOf`)
- `<@` (`subsetOf`)
- `&&` (`overlaps`)
- `in` (subquery)
- `notIn` (subquery)


## Select queries

- `leftJoin`
- `innerJoin`
- `where`
- `groupBy`
- `having`
- `sortBy`
- `offset`
- `limit`
- `union`
- `unionAll`


## Distinct clause

- `takeDistinct` (select queries)
- `Count.distinct`
- `Avg.distinct`
- `Sum.distinct`

## Commands

- `Insert`
- `Update`
