package day17

object Puzzle2 extends App {
  val topLeft = (138, -71)
  val bottomRight = (184, -125)
  val velocities = countVelocities
  println(velocities)

  private def countVelocities: Int = {
    val xVelocities = LazyList.range(0, 1000)
    xVelocities.map { initialXVelocity =>
      val yVelocities = LazyList.range(-1000, 1000)
      yVelocities
        .map(trajectoryPositions(initialXVelocity, _))
        .count(containsPositionInTargetArea)
    }.sum
  }

  private def trajectoryPositions(initialXVelocity: Int, initialYVelocity: Int) = {
    var xVelocity = initialXVelocity
    var yVelocity = initialYVelocity
    LazyList.unfold((0, 0)) {
      case (x, y) => Option.when(x <= bottomRight._1 && y >= bottomRight._2) {
        val nextX = x + xVelocity
        val nextY = y + yVelocity
        xVelocity = (xVelocity - 1).max(0)
        yVelocity = yVelocity - 1
        val position = (nextX, nextY)
        (position, position)
      }
    }
  }

  private def containsPositionInTargetArea(value: LazyList[(Int, Int)]) =
    value.exists {
      case (x, y) => x >= topLeft._1 && x <= bottomRight._1 && y <= topLeft._2 && y >= bottomRight._2
    }
}
