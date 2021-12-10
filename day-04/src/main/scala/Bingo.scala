final case class Board(numbers: Vector[Vector[Int]], playedNumbers: Vector[Int]) {

  def play(number: Int): Board =
    if (isWinner) this else
    copy(playedNumbers = playedNumbers :+ number)

  def play(numbers: Int*): Board =
    numbers.foldLeft(this)(_.play(_))

  def isWinner: Boolean =
    numbers.exists(_.forall(playedNumbers.contains)) ||
      numbers.transpose.exists(_.forall(playedNumbers.contains))

  def score: Option[Int] =
    Option.when(isWinner)((numbers.flatten.toSet -- playedNumbers.toSet).sum * playedNumbers.last)
}

object Board {

  def apply(numbers: Vector[Vector[Int]]): Board = new Board(numbers, Vector.empty)
}

final case class Game(boards: List[Board]) {

  def play(number: Int): Game =
    copy(boards = boards.map(_.play(number)))

  def play(numbers: Int*): LazyList[Game] =
    LazyList.from(numbers).scanLeft(this)(_.play(_))

  def winners: List[Board] =
    boards.filter(_.isWinner)

  def firstWinnerScore(numbers: List[Int]): Option[Int] =
    for {
      game        <- play(numbers: _*).lastOption
      firstWinner <- game.winners.minByOption(_.playedNumbers.length)
      score       <- firstWinner.score
    } yield score

  def lastWinnerScore(numbers: List[Int]): Option[Int] =
    for {
      game        <- play(numbers: _*).lastOption
      lastWinner  <- game.winners.maxByOption(_.playedNumbers.length)
      score       <- lastWinner.score
    } yield score
}

object Parser {

  import atto._
  import Atto._

  private val numbers: Parser[List[Int]] =
    sepBy(int, char(','))

  private val board: Parser[Board] = {

    val number: Parser[Int] =
      digit.manyN(2).map(_.mkString.toInt) |
        (char(' ') ~> digit.map(_.asDigit))

    manyN(5, manyN(5, number <~ opt(char(' '))) <~ opt(char('\n'))).map { matrix =>
      Board(matrix.map(_.toVector).toVector)
    }
  }

  private val game: Parser[Game] =
    sepBy(board, string("\n")).map(Game)

  private val parser: Parser[(List[Int], Game)] =
    (numbers <~ string("\n\n")) ~ game

  def parse(input: String): (List[Int], Game) =
    parser.parseOnly(input).done.option.get
}
