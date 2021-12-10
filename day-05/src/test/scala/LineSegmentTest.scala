import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class LineSegmentTest extends AnyFreeSpec with Matchers {

  "isOrthogonal" - {

    "must return true if both points are at the same x coordinate" in {
      LineSegment(Point(1, 2), Point(1, 3)).isOrthogonal mustEqual true
    }

    "must return true if both points are at the same y coordinate" in {
      LineSegment(Point(1, 2), Point(3, 2)).isOrthogonal mustEqual true
    }

    "must return false if points are not at the same x or y coordinate" in {
      LineSegment(Point(1, 2), Point(3, 4)).isOrthogonal mustEqual false
    }
  }

  "isDiagonal" - {

    "must return true if the x and y points are diagonal" in {
      LineSegment(Point(1, 1), Point(3, 3)).isDiagonal mustEqual true
    }

    "must return false for orthogonal lines" in {
      LineSegment(Point(1, 2), Point(1, 3)).isDiagonal mustEqual false
    }

    "must return false for non-diagonal lines" in {
      LineSegment(Point(1, 2), Point(2, 5)).isDiagonal mustEqual false
    }
  }

  "points" - {

    "must return a single point when both points are the same" in {
      LineSegment(Point(1, 1), Point(1, 1)).points must contain only Point(1, 1)
    }

    "must return all points included in the segment" in {
      LineSegment(Point(1, 1), Point(1, 3)).points must contain only(
        Point(1, 1), Point(1, 2), Point(1, 3)
      )
      LineSegment(Point(1, 1), Point(3, 1)).points must contain only(
        Point(1, 1), Point(2, 1), Point(3, 1)
      )
    }

    "must return points for lines with backwards points" in {
      LineSegment(Point(1, 3), Point(1, 1)).points must contain only(
        Point(1, 1), Point(1, 2), Point(1, 3)
      )
    }

    "must return points for diagonal lines" in {
      LineSegment(Point(1, 1), Point(3, 3)).points must contain only(
        Point(1, 1), Point(2, 2), Point(3, 3)
      )
      LineSegment(Point(3, 3), Point(1, 1)).points must contain only(
        Point(1, 1), Point(2, 2), Point(3, 3)
      )
      LineSegment(Point(1, 3), Point(3, 1)).points must contain only(
        Point(1, 3), Point(2, 2), Point(3, 1)
      )
    }
  }
}
