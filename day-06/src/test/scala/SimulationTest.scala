import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.io.Source

class SimulationTest extends AnyFreeSpec with Matchers {

  "count" - {

    "must return the number of fish in the simulation" in {
      Simulation(List(1, 2)).count mustEqual 2
      Simulation(List(1, 2, 3)).count mustEqual 3
    }
  }

  "simulate" - {

    "must decrease the counter of each fish in the simulation" in {
      Simulation(List(1, 2)).simulate.fish must contain only(
        0, 1
      )
    }

    "must spawn a new fish and reset the fish's counter when it is 0" in {
      Simulation(List(0)).simulate.fish must contain only(
        6, 8
      )
    }

    "must return the correct count for the test input" in {
      Simulation(List(3, 4, 3, 1, 2)).simulate(18) mustEqual 26
      Simulation(List(3, 4, 3, 1, 2)).simulate(80) mustEqual 5934
      Simulation(List(3, 4, 3, 1, 2)).simulate(256) mustEqual 26984457539L
    }

    "must return the correct count for the puzzle input" in {
      val simulation = Parser.parse(Source.fromResource("input.txt").mkString)
      simulation.simulate(80) mustEqual 390923
      simulation.simulate(256) mustEqual 1749945484935L
    }
  }
}
