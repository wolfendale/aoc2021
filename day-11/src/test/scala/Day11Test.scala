import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day11Test extends AnyFreeSpec with Matchers {

  private val testInput = """5483143223
                            |2745854711
                            |5264556173
                            |6141336146
                            |6357385478
                            |4167524645
                            |2176841721
                            |6882881134
                            |4846848554
                            |5283751526""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for the test input" in {
      Day11.part1(testInput) mustEqual 1656
    }

    "must work for the puzzle input" in {
      Day11.part1(puzzleInput) mustEqual 1655
    }
  }

  "part 2" - {

    "must work for the test input" in {
      Day11.part2(testInput) mustEqual 195
    }

    "must work for the puzzle input" in {
      Day11.part2(puzzleInput) mustEqual 337
    }
  }
}
