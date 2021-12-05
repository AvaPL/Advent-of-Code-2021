package day5

import scala.math.{abs, max, min, signum}

case class Line(start: Point, end: Point) {

  lazy val isHorizontal: Boolean = start.x == end.x

  lazy val isVertical: Boolean = start.y == end.y

  lazy val isDiagonal: Boolean = abs(start.x - end.x) == abs(start.y - end.y)

  lazy val coveredPoints: Seq[Point] =
    if (isHorizontal) horizontalCoveredPoints
    else if (isVertical) verticalCoveredPoints
    else if (isDiagonal) diagonalCoveredPoints
    else Seq.empty

  private lazy val horizontalCoveredPoints =
    min(start.y, end.y) to max(start.y, end.y) map (Point(start.x, _))

  private lazy val verticalCoveredPoints =
    min(start.x, end.x) to max(start.x, end.x) map (Point(_, start.y))

  private lazy val diagonalCoveredPoints = {
    val stepX = signum(end.x - start.x)
    val stepY = signum(end.y - start.y)
    val stepPoint = Point(stepX, stepY)
    Seq.unfold(start) { point =>
      Option.when(point != end) {
        (point, point.add(stepPoint))
      }
    } :+ end
  }
}
