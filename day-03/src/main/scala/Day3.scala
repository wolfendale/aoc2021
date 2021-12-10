import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object Day3 {

  def calculatePowerConsumption(input: List[String]): Int = {

    val width = input.head.length

    val gamma = input.transpose.map {
      _.groupMapReduce(identity)(_ => 1)(_ + _).maxBy(_._2)._1
    }.mkString pipe (Integer.parseInt(_, 2))

    val epsilon = gamma ^ Integer.parseInt("1" * width, 2)

    gamma * epsilon
  }

  def calculateLifeSupportRating(input: List[String]): Int = {

    @tailrec
    def calculateOxygenGeneratorRating(input: List[String], i: Int = 0): Int = {
      val counts = input.groupMapReduce(_(i))(_ => 1)(_ + _)
      val median = if (counts('1') == counts('0')) '1' else counts.maxBy(_._2)._1
      val selected = input.filter(_(i) == median)
      if (selected.length == 1) Integer.parseInt(selected.head, 2) else {
        calculateOxygenGeneratorRating(selected, i + 1)
      }
    }

    @tailrec
    def calculateCo2ScrubbingRating(input: List[String], i: Int = 0): Int = {
      val counts = input.groupMapReduce(_(i))(_ => 1)(_ + _)
      val median = if (counts('1') == counts('0')) '0' else counts.minBy(_._2)._1
      val selected = input.filter(_(i) == median)
      if (selected.length == 1) Integer.parseInt(selected.head, 2) else {
        calculateCo2ScrubbingRating(selected, i + 1)
      }
    }

    calculateOxygenGeneratorRating(input) * calculateCo2ScrubbingRating(input)
  }
}
