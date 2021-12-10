import cats._
import cats.implicits._

object Day8 {

  def countUniqueDigits(input: List[Entry]): Int =
    input.foldMap(entry => entry.output.count(digit => Set(2, 3, 4, 7).contains(digit.size)))

  def decode(input: List[Entry]): Int = {
    input.foldMap { entry =>
      val decoder = generateMapping(entry.combinations)
      entry.output.traverse(_.decode(decoder).toInt).map(_.mkString.toInt).getOrElse(0)
    }
  }

  private def frequency(digits: Set[Set[Segment]]): Map[Segment, Int] =
    digits.foldLeft(Map.empty[Segment, Int]) { (m, n) =>
      m |+| n.groupMapReduce(identity)(_ => 1)(_ + _)
    }

  private val defaultFrequencies: Map[Segment, Int] =
    frequency(Display.values.map(_.segments))

  private def mappings(digits: Set[Display]): Segment => Set[Segment] = {

    val inverseFrequencyMap = frequency(digits.map(_.segments))
      .groupMapReduce(_._2)(a => Set(a._1))(_ ++ _)
      .withDefaultValue(Set.empty)

    defaultFrequencies andThen inverseFrequencyMap
  }

  private def generateMapping(values: Set[Display]): Segment => Segment = {
    val frequencyMap = mappings(values)
    val one = values.find(_.size == 2).getOrElse(Display.Empty).segments
    val seven = values.find(_.size == 3).getOrElse(Display.Empty).segments
    val four = values.find(_.size == 4).getOrElse(Display.Empty).segments
    val eight = values.find(_.size == 7).getOrElse(Display.Empty).segments
    for {
      a <- frequencyMap(Segment.A)
      b <- frequencyMap(Segment.B)
      c <- frequencyMap(Segment.C)
      d <- frequencyMap(Segment.D)
      e <- frequencyMap(Segment.E)
      f <- frequencyMap(Segment.F)
      g <- frequencyMap(Segment.G)
      if (seven -- one).contains(a)
      if (four -- one - b).contains(d)
      if (one - f).contains(c)
      if (eight -- four - a - e).contains(g)
    } yield {
      Map(
        a -> Segment.A,
        b -> Segment.B,
        c -> Segment.C,
        d -> Segment.D,
        e -> Segment.E,
        f -> Segment.F,
        g -> Segment.G
      )
    }
  }.head
}
