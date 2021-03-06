object Day1 {

  def part1(lines: List[Int]): Int =
    lines match {
      case a :: b :: xs if a < b => part1(b :: xs) + 1
      case _ :: a :: xs          => part1(a :: xs)
      case _                     => 0
    }

//  def part1(lines: List[Int]): Int =
//      lines
//        .sliding(2, 1)
//        .count({ case Seq(a, b) => a < b })

  def part2(lines: List[Int]): Int =
    lines
      .sliding(3, 1)
      .map(_.sum)
      .sliding(2, 1)
      .count({ case Seq(a, b) => a < b})
}
