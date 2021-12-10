import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day5Test extends AnyFreeSpec with Matchers {

  val testInput = """0,9 -> 5,9
                    |8,0 -> 0,8
                    |9,4 -> 3,4
                    |2,2 -> 2,1
                    |7,0 -> 7,4
                    |6,4 -> 2,0
                    |0,9 -> 2,9
                    |3,4 -> 1,4
                    |0,0 -> 8,8
                    |5,5 -> 8,2""".stripMargin

  "findDangerousOrthogonalPoints" - {

    "must find all points where 2 or more lines intersect" in {
      val input = Parser.parse(testInput)
      Day5.findDangerousOrthogonalPoints(input) mustEqual 5
    }

    "must work for puzzle input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day5.findDangerousOrthogonalPoints(input) mustEqual 5774
    }
  }

  "findDangerousPoints" - {

    "must find all points where 2 or more lines intersect" in {
      val input = Parser.parse(testInput)
      Day5.findDangerousPoints(input) mustEqual 12
    }

    "must work for puzzle input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day5.findDangerousPoints(input) mustEqual 18423
    }
  }
}
