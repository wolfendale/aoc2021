import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day8Test extends AnyFreeSpec with Matchers {

  val testInput = Parser.parse(Source.fromResource("test.txt").mkString)
  val input = Parser.parse(Source.fromResource("input.txt").mkString)

  "countUniqueDigits" - {

    "must return correctly for the test input" in {
      Day8.countUniqueDigits(testInput) mustEqual 26
    }

    "must return correctly for the puzzle input" in {
      Day8.countUniqueDigits(input) mustEqual 365
    }
  }

  "decode" - {

    "must return correctly for the test input" in {
      Day8.decode(testInput) mustEqual 61229
    }

    "must return correctly for the puzzle input" in {
      Day8.decode(input) mustEqual 975706
    }
  }
}
