package tydal.test.aggregations

import org.scalatest.freespec._
import org.scalatest.matchers._
import skunk.{Query, Void}
import tydal.Schema._
import tydal._
import tydal.compiler._
import tydal.test.IntegrationTesting

class CountSpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "Aggregation (COUNT)" in {

    testUnique(
      Select.from(
        (
          Select("hello"[varchar] as "bar") union
            Select("world"[varchar]) union
            Select("!!"[varchar])
        ) as "foo"
      )
      .take(x => Count(x("foo", "bar")))
      .compile,
      "SELECT COUNT(foo.bar) FROM (SELECT $1 AS bar UNION SELECT $2 UNION SELECT $3) foo",
      3
    )
  }
