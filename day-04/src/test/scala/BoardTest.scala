import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class BoardTest extends AnyFreeSpec with Matchers {

  "isWinner" - {

    val board = Board(Vector(Vector(1, 2), Vector(3, 4)))

    "must return true if a row is filled" in {
      board.play(1, 2).isWinner mustEqual true
    }

    "must return true if a column is filled" in {
      board.play(1, 3).isWinner mustEqual true
    }

    "must return false if no row or column is filled" in {
      board.isWinner mustEqual false
      board.play(1, 4).isWinner mustEqual false
      board.play(2, 3).isWinner mustEqual false
    }
  }
}
