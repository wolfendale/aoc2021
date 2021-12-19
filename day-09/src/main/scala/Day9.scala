import cats._
import cats.implicits._
import scala.annotation.tailrec

object Day9 {

  def calculateRiskValue(heightMap: Vector[Vector[Int]]): Int = {

    def vonNeumannNeighbourhood(x: Int, y: Int): List[Int] = {
      def get(x: Int, y: Int): Option[Int] = for {
        row   <- heightMap.lift(y)
        value <- row.lift(x)
      } yield value
      List(
        get(x - 1, y),
        get(x + 1, y),
        get(x, y - 1),
        get(x, y + 1)
      ).flatten
    }

    heightMap.zipWithIndex.foldLeft(0) { case (m, (row, y)) =>
      row.zipWithIndex.foldLeft(m) { case (m, (height, x)) =>
        val isLowPoint = vonNeumannNeighbourhood(x, y).forall(_ > height)
        if (isLowPoint) m + 1 + height else m
      }
    }
  }

  def findBasins(heightMap: Vector[Vector[Int]]): Int = {

    def vonNeumannNeighbourhood(x: Int, y: Int): List[(Int, Int)] =
      List(
        (x - 1) -> y,
        (x + 1) -> y,
        x -> (y - 1),
        x -> (y + 1)
      )

    def get(x: Int, y: Int): Option[Int] = for {
      row    <- heightMap.lift(y)
      height <- row.lift(x)
    } yield height

    def lowestPoint(x: Int, y: Int): Option[(Int, Int)] = {

      get(x, y).flatMap { height =>
        if (height == 9) {
          None
        } else {

          val neighbours = for {
            (x2, y2) <- vonNeumannNeighbourhood(x, y)
            height2  <- get(x2, y2)
            if height2 < height
            lowest   <- lowestPoint(x2, y2)
          } yield (lowest, height2)

          neighbours
            .minByOption(_._2)
            .map(_._1)
            .orElse(Some((x, y)))
        }
      }
    }

    val basins = heightMap.zipWithIndex.foldLeft(Map.empty[(Int, Int), Int]) { case (m, (row, y)) =>
      row.zipWithIndex.foldLeft(m) { case (m, (_, x)) =>
        m |+| lowestPoint(x, y).map(point => Map(point -> 1)).getOrElse(Map.empty)
      }
    }

    basins
      .values
      .toList
      .sorted
      .reverse
      .take(3)
      .product
  }
}

object Parser {

  import atto._
  import Atto._

  private val parser: Parser[Vector[Vector[Int]]] =
    sepBy(many(digit.map(_.asDigit)), char('\n')).map(_.map(_.toVector).toVector)

  def parse(input: String): Vector[Vector[Int]] =
    parser.parseOnly(input).done.option.get
}
