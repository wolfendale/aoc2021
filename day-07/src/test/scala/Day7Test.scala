import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day7Test extends AnyFreeSpec with Matchers {

  "calculateFuelConsumption" - {

    "must return 0 for no submarines" in {
      Day7.calculateLinearFuelConsumption(List.empty) mustEqual 0
    }

    "must return 0 for a single submarine" in {
      Day7.calculateLinearFuelConsumption(List(1)) mustEqual 0
      Day7.calculateLinearFuelConsumption(List(1337)) mustEqual 0
    }

    "must return the right value for the test input" in {
      Day7.calculateLinearFuelConsumption(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)) mustEqual 37
    }

    "must return the right value for the puzzle input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day7.calculateLinearFuelConsumption(input) mustEqual 347449
    }
  }

  "calculateComplexFuelConsumption" - {

    "must return the right value for the test input" in {
      Day7.calculateComplexFuelConsumption(List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)) mustEqual 168
    }

    "must return the right value for the puzzle input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day7.calculateComplexFuelConsumption(input) mustEqual 98039527
    }
  }
}
