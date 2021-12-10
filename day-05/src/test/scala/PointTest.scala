import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class PointTest extends AnyFreeSpec with Matchers {

  "abs" - {

    "must return the absolute equivalent point" in {
      Point(1, 2).abs mustEqual Point(1, 2)
      Point(-1, -2).abs mustEqual Point(1, 2)
    }
  }

  "-" - {

    "must return the difference between two points" in {
      Point(1, 2) - Point(1, 2) mustEqual Point(0, 0)
    }
  }
}
