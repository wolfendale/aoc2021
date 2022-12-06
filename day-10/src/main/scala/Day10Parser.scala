import atto.Atto._
import atto._

sealed trait Chunk {

  def chunks: Seq[Chunk]

  lazy val allChunks: Seq[Chunk] =
    this +: chunks.flatMap(_.allChunks)
}

object Chunk {

  final case class Valid(open: Char, close: Char, chunks: Seq[Chunk] = Seq.empty) extends Chunk {
    override def toString: String = s"${Console.WHITE}$open${chunks.mkString}$close${Console.WHITE}"
  }

  final case class Corrupt(open: Char, close: Char, chunks: Seq[Chunk] = Seq.empty) extends Chunk {
    override def toString: String = s"${Console.RED}$open${chunks.mkString}$close${Console.WHITE}"
  }

  final case class Incomplete(open: Char, close: Char, chunks: Seq[Chunk] = Seq.empty) extends Chunk {
    override def toString: String = s"${Console.YELLOW}$open${chunks.mkString}${Console.WHITE}"
  }
}

final case class Line(chunks: Seq[Chunk]) {

  lazy val allChunks: Seq[Chunk] = chunks.flatMap(_.allChunks)
  lazy val corrupted: Seq[Chunk.Corrupt] = allChunks.collect { case c: Chunk.Corrupt => c }
  lazy val incomplete: Seq[Chunk.Incomplete] = allChunks.collect { case c: Chunk.Incomplete => c }

  override def toString: String = chunks.mkString
}

object Day10Parser {

  private val brackets: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  private lazy val open: Parser[Char] =
    oneOf("([{<")

  private def valid(open: Char, chunks: Seq[Chunk]): Parser[Chunk] =
    char(brackets(open))
      .map(close => Chunk.Valid(open, close, chunks))

  private def corrupt(open: Char, chunks: Seq[Chunk]): Parser[Chunk] =
    oneOf((brackets.values.toSet - brackets(open)).mkString)
      .map(close => Chunk.Corrupt(open, close, chunks))

  private lazy val chunk: Parser[Chunk] =
    for {
      open   <- open
      chunks <- chunks
      chunk  <- opt(valid(open, chunks) | corrupt(open, chunks))
        .map(_.getOrElse(Chunk.Incomplete(open, brackets(open), chunks)))
    } yield chunk

  private lazy val chunks: Parser[Seq[Chunk]] =
    many(chunk)

  private lazy val line: Parser[Line] =
    chunks.map(Line)

  private val newline = char('\n') | char('\r')

  private val parser =
    line.sepBy(newline)

  def parse(input: String): Seq[Line] =
    parser.parseOnly(input).done.option.get
}
