import scala.util.chaining.scalaUtilChainingOps

object Day10 {

  def corruptedScore(input: String): Int = {

     val score: Map[Char, Int] =
      Map(
        ')' -> 3,
        ']' -> 57,
        '}' -> 1197,
        '>' -> 25137
      )

    Day10Parser
      .parse(input)
      .tap(_.foreach(println))
      .flatMap { line =>
        line.corrupted.map(chunk => score(chunk.close))
      }.sum
  }

  def autoCompleteScore(input: String): Long = {

    val score: Map[Char, Int] =
      Map(
        ')' -> 1,
        ']' -> 2,
        '}' -> 3,
        '>' -> 4
      )

    val scores = Day10Parser
      .parse(input)
      .tap(_.foreach(println))
      .filter(_.corrupted.isEmpty)
      .map { line =>
        line.incomplete.foldRight(0L) { (n, m) =>
          (m * 5) + score(n.close)
        }
      }.sorted

    scores(scores.length / 2)
  }
}
