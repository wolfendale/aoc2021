import cats._
import cats.data._
import cats.implicits._

sealed trait Direction

object Direction {
  case object Forward extends Direction
  case object Up extends Direction
  case object Down extends Direction
}

final case class Command(direction: Direction, distance: Int)

object Parser {

  import atto._
  import Atto._

  private val direction: Parser[Direction] =
    string("forward").as(Direction.Forward) |
      string("up").as(Direction.Up) |
      string("down").as(Direction.Down)

  private val command: Parser[Command] =
    (direction <~ char(' '), int).mapN(Command)

  private val parser: Parser[List[Command]] =
    sepBy(command, char('\n'))

  def parse(input: String): List[Command] =
    parser.parseOnly(input).done.option.get
}

