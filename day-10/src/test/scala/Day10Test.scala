import org.scalatest.OptionValues
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day10Test extends AnyFreeSpec with Matchers with OptionValues {

  private val testInput = """[({(<(())[]>[[{[]{<()<>>
                |[(()[<>])]({[<{<<[]>>(
                |{([(<{}[<>[]}>{[]{[(<()>
                |(((({<>}<{<{<>}{[]{[]{}
                |[[<[([]))<([[{}[[()]]]
                |[{[{({}]{}}([{[{{{}}([]
                |{<[[]]>}<{[{[{[]{()[[[]
                |[<(<(<(<{}))><([]([]()
                |<{([([[(<>()){}]>(<<{{
                |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "corruptedScore" - {

    "must work for test input" in {
      Day10.corruptedScore(testInput) mustEqual 26397
    }

    "must work for the puzzle input" in {
      Day10.corruptedScore(puzzleInput) mustEqual 167379
    }
  }

  "autocompleteScore" - {

    "must work for the test input" in {
      Day10.autoCompleteScore(testInput) mustEqual 288957
    }

    "must work for the puzzle input" in {
      Day10.autoCompleteScore(puzzleInput) mustEqual 0
    }
  }
}
