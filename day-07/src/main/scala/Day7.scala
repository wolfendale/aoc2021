object Day7 {

  def calculateLinearFuelConsumption(submarines: List[Int]): Int = {

    def median(list: List[Int]): Option[Int] = {
      val length = list.length
      val sorted = list.sorted
      length match {
        case 0               => None
        case n if n % 2 == 0 => Some((sorted((n / 2) - 1) + sorted(n / 2)) / 2)
        case n               => Some(sorted(n / 2))
      }
    }

    val cheapest = median(submarines).getOrElse(0)

    submarines.foldLeft(0) { (m, n) =>
      m + (n - cheapest).abs
    }
  }

  def calculateComplexFuelConsumption(submarines: List[Int]): Int = {
    val sorted = submarines.sorted
    val max = sorted.lastOption.getOrElse(0)
    val min = sorted.headOption.getOrElse(0)
    (min to max).map { target =>
      submarines.foldLeft(0) { (m, n) =>
        val distance = (target - n).abs
        m + (distance * (distance + 1) / 2)
      }
    }.min
  }
}

object Parser {

  import atto._
  import Atto._

  private val parser: Parser[List[Int]] =
    sepBy(int, char(','))

  def parse(input: String): List[Int] =
    parser.parseOnly(input).done.option.get
}