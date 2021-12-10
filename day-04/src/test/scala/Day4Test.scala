import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day4Test extends AnyFreeSpec with Matchers with OptionValues {

  val testInput: String =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7
      |""".stripMargin

  "firstWinnerScore" - {

    "must return the first winning board's score" in {
      val (numbers, game) = Parser.parse(testInput)
      game.firstWinnerScore(numbers).value mustEqual 4512
    }

    "must work for the puzzle input" in {
      val (numbers, game) = Parser.parse(Source.fromResource("input.txt").mkString)
      game.firstWinnerScore(numbers).value mustEqual 28082
    }
  }

  "lastWinnerScore" - {

    "must return the last winning board's score" in {
      val (numbers, game) = Parser.parse(testInput)
      game.lastWinnerScore(numbers).value mustEqual 1924
    }

    "must work for the puzzle input" in {
      val (numbers, game) = Parser.parse(Source.fromResource("input.txt").mkString)
      game.lastWinnerScore(numbers).value mustEqual 8224
    }
  }
}
