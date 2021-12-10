object Day5 {

  def findDangerousOrthogonalPoints(lines: List[LineSegment]): Int = {

    val points = for {
      line  <- lines.filter(_.isOrthogonal)
      point <- line.points
    } yield point

    points
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 > 1)
  }

  def findDangerousPoints(lines: List[LineSegment]): Int = {

    val points = for {
      line  <- lines.filter(l => l.isOrthogonal || l.isDiagonal)
      point <- line.points
    } yield point

    points
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .count(_._2 > 1)
  }
}
