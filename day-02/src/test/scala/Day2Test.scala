import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day2Test extends AnyFreeSpec with Matchers {

  "part 1" - {

    "must return the product of the position after applying the commands" in {

      val input = List(
        Command(Direction.Forward, 5),
        Command(Direction.Down, 5),
        Command(Direction.Forward, 8),
        Command(Direction.Up, 3),
        Command(Direction.Down, 8),
        Command(Direction.Forward, 2)
      )

      Day2.part1(input) mustEqual 150
    }

    "must work for the input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day2.part1(input) mustEqual 1694130
    }
  }

  "part 2" - {

    "must return the product of the position after applying the commands" in {

      val input = List(
        Command(Direction.Forward, 5),
        Command(Direction.Down, 5),
        Command(Direction.Forward, 8),
        Command(Direction.Up, 3),
        Command(Direction.Down, 8),
        Command(Direction.Forward, 2)
      )

      Day2.part2(input) mustEqual 900
    }

    "must work for the input" in {
      val input = Parser.parse(Source.fromResource("input.txt").mkString)
      Day2.part2(input) mustEqual 1698850445
    }
  }
}
