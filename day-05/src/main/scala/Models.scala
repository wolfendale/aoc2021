import cats._
import cats.data._
import cats.implicits._

import scala.annotation.tailrec

final case class Point(x: Int, y: Int) {

  def -(other: Point): Point =
    Point(x - other.x, y - other.y)

  def abs: Point =
    Point(x.abs, y.abs)
}

final case class LineSegment(a: Point, b: Point) {

  def points: List[Point] = {

    @tailrec
    def range(from: Int, to: Int, points: Seq[Int] = Seq.empty): Seq[Int] = {
      if (from == to) points :+ to else {
        if (from < to) {
          range(from + 1, to, points :+ from)
        } else {
          range(from - 1, to, points :+ from)
        }
      }
    }

    range(a.x, b.x).zipAll(range(a.y, b.y), a.x.min(b.x), a.y.min(b.y)).map(Point.tupled).toList
  }

  def isOrthogonal: Boolean =
    a.x == b.x || a.y == b.y

  def isDiagonal: Boolean = {
    val p = (a - b).abs
    p.x == p.y
  }
}

object Parser {

  import atto._
  import Atto._

  private val point: Parser[Point] =
    (int <~ char(','), int).mapN(Point)

  private val lineSegment: Parser[LineSegment] =
    (point <~ string(" -> "), point).mapN(LineSegment)

  private val parser: Parser[List[LineSegment]] =
    sepBy(lineSegment, char('\n'))

  def parse(input: String): List[LineSegment] =
    parser.parseOnly(input).done.option.get
}
