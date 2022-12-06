import cats.implicits._

import scala.util.chaining.scalaUtilChainingOps

object Day11 {

  def part1(input: String): Int =
    (1 to 100).foldLeft(Day11Parser.parse(input)) { (m, _) =>
      m.step
    }.flashes

  def part2(input: String): Int =
    steps(Day11Parser.parse(input)).takeWhile(!_.isBright).size

  private def steps(grid: OctopiGrid): LazyList[OctopiGrid] =
    LazyList.iterate(grid)(_.step)
}

final case class Octopus(energy: Int) {
  def charge: Octopus = copy(energy = energy + 1)
}

final case class OctopiGrid(grid: Vector[Vector[Octopus]], flashes: Int) {

  private def get(x: Int, y: Int): Option[Octopus] =
    for {
      row     <- grid.lift(y)
      octopus <- row.lift(x)
    } yield octopus

  private def updateGrid(x: Int, y: Int, f: Octopus => OctopiGrid): OctopiGrid =
    get(x, y).map(f).getOrElse(this)

  private def update(x: Int, y: Int, f: Octopus => Octopus): OctopiGrid =
    updateGrid(x, y, octopus => copy(grid.updated(y, grid(y).updated(x, f(octopus)))))

  private def flash: OctopiGrid =
    copy(flashes = flashes + 1)

  private def charge(x: Int, y: Int): OctopiGrid =
    update(x, y, _.charge)
      .pipe { g =>
        g.updateGrid(x, y, octopus =>
          if (octopus.energy == 10) {
            (-1 to 1).foldLeft(g.flash) { (g, i) =>
              (-1 to 1).foldLeft(g) { (g, j) =>
                g.charge(x + i, y + j)
              }
            }
          } else g
        )
      }

  private def forEach(f: (Int, Int, OctopiGrid) => OctopiGrid): OctopiGrid =
    grid.zipWithIndex.foldLeft(this) { case (g, (row, y)) =>
      row.zipWithIndex.foldLeft(g) { case (g, (_, x)) =>
        f(x, y, g)
      }
    }

  private def charge: OctopiGrid = forEach { (x, y, g) =>
    g.charge(x, y)
  }

  private def reset: OctopiGrid = forEach { (x, y, g) =>
    g.update(x, y, octopus => if (octopus.energy > 9) Octopus(0) else octopus)
  }

  def step: OctopiGrid =
    charge.reset

  def isBright: Boolean =
    grid.forall(_.forall(_.energy == 0))
}

object Day11Parser {

  import atto._
  import Atto._

  private val octopus = digit.map(d => Octopus(d.asDigit))
  private val newline = char('\r') | char('\n')
  private val grid = octopus.many.map(_.toVector)
    .sepBy(newline).map(rows => OctopiGrid(rows.toVector, 0))

  def parse(input: String): OctopiGrid =
    grid.parseOnly(input).done.option.get
}