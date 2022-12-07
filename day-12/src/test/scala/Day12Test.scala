import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class Day12Test extends AnyFreeSpec with Matchers {

  private val testInput1 = """start-A
                            |start-b
                            |A-c
                            |A-b
                            |b-d
                            |A-end
                            |b-end""".stripMargin

  private val testInput2 = """dc-end
                             |HN-start
                             |start-kj
                             |dc-start
                             |dc-HN
                             |LN-dc
                             |HN-end
                             |kj-sa
                             |kj-HN
                             |kj-dc""".stripMargin

  private val testInput3 = """fs-end
                             |he-DX
                             |fs-he
                             |start-DX
                             |pj-DX
                             |end-zg
                             |zg-sl
                             |zg-pj
                             |pj-he
                             |RW-he
                             |fs-DX
                             |pj-RW
                             |zg-RW
                             |start-pj
                             |he-WI
                             |zg-he
                             |pj-fs
                             |start-RW""".stripMargin

  private val puzzleInput = Source.fromResource("input.txt").mkString

  "part 1" - {

    "must work for test input 1" in {
      Day12.part1(testInput1) mustEqual 10
    }

    "must work for test input 2" in {
      Day12.part1(testInput2) mustEqual 19
    }

    "must work for test input 3" in {
      Day12.part1(testInput3) mustEqual 226
    }

    "must work for the puzzle input" in {
      Day12.part1(puzzleInput) mustEqual 4749
    }
  }

  "part 2" - {

    "must work for test input 1" in {
      Day12.part2(testInput1) mustEqual 36
    }

    "must work for test input 2" in {
      Day12.part2(testInput2) mustEqual 103
    }

    "must work for test input 3" in {
      Day12.part2(testInput3) mustEqual 3509
    }

    "must work for the puzzle input" in {
      Day12.part2(puzzleInput) mustEqual 123054
    }
  }
}
