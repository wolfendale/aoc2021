import cats._
import cats.data._
import cats.implicits._

object Day2 {

  def part1(commands: List[Command]): Int = {
    val (position, depth) = commands.foldLeft((0, 0)) {
      case ((position, depth), Command(Direction.Forward, x)) => (position + x, depth)
      case ((position, depth), Command(Direction.Up, y))      => (position    , depth - y)
      case ((position, depth), Command(Direction.Down, y))    => (position    , depth + y)
    }
    position * depth
  }

  def part2(commands: List[Command]): Int = {
    val (_, position, depth) = commands.foldLeft((0, 0, 0)) {
      case ((aim, position, depth), Command(Direction.Forward, x)) => (aim    , position + x, depth + aim * x)
      case ((aim, position, depth), Command(Direction.Up, y))      => (aim - y, position    , depth)
      case ((aim, position, depth), Command(Direction.Down, y))    => (aim + y, position    , depth)
    }
    position * depth
  }
}