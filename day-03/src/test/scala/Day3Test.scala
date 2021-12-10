import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day3Test extends AnyFreeSpec with Matchers {

  val testInput = List(
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
  )

  "calculatePowerConsumption" -{

    "must return power consumption of 198" in {
      Day3.calculatePowerConsumption(testInput) mustEqual 198
    }

    "must work for the puzzle input" in {
      val input = Source.fromResource("input.txt").getLines.toList
      Day3.calculatePowerConsumption(input) mustEqual 3885894
    }
  }

  "calculateLifeSupportRating" - {

    "must return a rating of 230" in {
      Day3.calculateLifeSupportRating(testInput) mustEqual 230
    }

    "must work for the puzzle input" in {
      val input = Source.fromResource("input.txt").getLines.toList
      Day3.calculateLifeSupportRating(input) mustEqual 4375225
    }
  }
}
