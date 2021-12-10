import Display.{Eight, Five, Four, Nine, One, Seven, Six, Three, Two, Zero}
import cats._
import cats.implicits._

sealed abstract class Segment extends Product with Serializable

object Segment {

  case object A extends Segment
  case object B extends Segment
  case object C extends Segment
  case object D extends Segment
  case object E extends Segment
  case object F extends Segment
  case object G extends Segment

  val values: Set[Segment] = Set(A, B, C, D, E, F, G)
}

final case class Display(segments: Set[Segment]) {

  def toInt: Option[Int] =
    this match {
      case Zero  => Some(0)
      case One   => Some(1)
      case Two   => Some(2)
      case Three => Some(3)
      case Four  => Some(4)
      case Five  => Some(5)
      case Six   => Some(6)
      case Seven => Some(7)
      case Eight => Some(8)
      case Nine  => Some(9)
      case _     => None
    }

  def size: Int =
    segments.size

  def decode(f: Segment => Segment): Display =
    Display(segments.map(f))
}

object Display {

  import Segment._

  val Zero: Display = Display(Set(A, B, C, E, F, G))
  val One: Display = Display(Set(C, F))
  val Two: Display = Display(Set(A, C, D, E, G))
  val Three: Display = Display(Set(A, C, D, F, G))
  val Four: Display = Display(Set(B, D, C, F))
  val Five: Display = Display(Set(A, B, D, F, G))
  val Six: Display = Display(Set(A, B, D, E, F, G))
  val Seven: Display = Display(Set(A, C, F))
  val Eight: Display = Display(Set(A, B, C, D, E, F, G))
  val Nine: Display = Display(Set(A, B, C, D, F, G))
  val Empty: Display = Display(Set.empty)

  val values: Set[Display] = Set(Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine)
}

final case class Entry(combinations: Set[Display], output: List[Display])

object Parser {

  import atto._
  import Atto._
  import Segment._

  private val segment: Parser[Segment] =
    char('a').as(A) |
      char('b').as(B) |
      char('c').as(C) |
      char('d').as(D) |
      char('e').as(E) |
      char('f').as(F) |
      char('g').as(G)

  private val display: Parser[Display] =
    many(segment).map(d => Display(d.toSet))

  private val entry: Parser[Entry] =
    (manyN(10, display <~ char(' ')).map(_.toSet) ~ (string("| ") ~> manyN(4, display <~ opt(char(' ')))))
      .map(Entry.tupled)

  private val parser: Parser[List[Entry]] =
    sepBy(entry, char('\n'))

  def parse(input: String): List[Entry] =
    parser.parseOnly(input).done.option.get
}
