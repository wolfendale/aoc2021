import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day9Test extends AnyFreeSpec with Matchers {

  val testInput = Parser.parse("""2199943210
                    |3987894921
                    |9856789892
                    |8767896789
                    |9899965678""".stripMargin)

  val input = Parser.parse(Source.fromResource("input.txt").mkString)

  "calculateRiskLevel" - {

    "must return the right value for the test input" in {
      Day9.calculateRiskValue(testInput) mustEqual 15
    }

    "must return the right value for the puzzle input" in {
      Day9.calculateRiskValue(input) mustEqual 417
    }
  }

  "findBasins" - {

    "must return the right value for the test input" in {
      Day9.findBasins(testInput) mustEqual 1134
    }

    "must return the right value for the puzzle input" in {
      Day9.findBasins(input) mustEqual 1148965
    }
  }
}
