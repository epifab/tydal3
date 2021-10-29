package tydal.test.arithmetics

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should
import tydal.test.IntegrationTesting
import tydal._

class ArithmeticsPrioritySpec extends AnyFreeSpec with should.Matchers with IntegrationTesting:

  "a * (b + c) - d / e" in {
    testUnique(
      Select(5[int4] * <<(2[int4] + 3[int4]) - 6[int4] / 2[int4]).compile,
      "SELECT $1 * ($2 + $3) - $4 / $5",
      22
    )
  }
