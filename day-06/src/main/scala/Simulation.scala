import cats._
import cats.implicits._

final case class Simulation(private val counts: Map[Int, Long]) {

  def count: Long = counts.values.sum

  def fish: List[Int] = counts.foldLeft(List.empty[Int]) {
    case (list, (number, count)) => List.fill(count.toInt)(number) ++ list
  }

  def simulate: Simulation = Simulation {
    counts.foldLeft(Map.empty[Int, Long]) {
      case (m, (0, count)) => m |+| Map(6 -> count, 8 -> count)
      case (m, (n, count)) => m |+| Map(n - 1 -> count)
    }
  }

  def simulate(days: Int): Long =
    LazyList.iterate(this)(_.simulate).take(days + 1).last.count
}

object Simulation {

  def apply(input: List[Int]): Simulation =
    new Simulation(input.groupMapReduce(identity)(_ => 1L)(_ + _))
}

object Parser {

  import atto._
  import Atto._

  private val parser: Parser[Simulation] =
    sepBy(int, char(',')).map(Simulation.apply)

  def parse(input: String): Simulation =
    parser.parseOnly(input).done.option.get
}