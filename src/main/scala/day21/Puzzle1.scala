package day21

import scala.collection.mutable.ListBuffer

object Puzzle1 extends App {
  val p1StartPosition = 3
  val p2StartPosition = 7
  val p1Rolls = List(6, 4, 2, 0, 8)
  val p2Rolls = List(5, 3, 1, 9, 7)
  val p1Positions = playerPositions(p1StartPosition, p1Rolls)
  val p2Positions = playerPositions(p2StartPosition, p2Rolls ++ p2Rolls)
  val ((p1Score, p2Score), rollsCount) = playGame
  val result = p1Score.min(p2Score) * rollsCount
  println(result)

  private def playerPositions(startPosition: Int, rolls: List[Int]) = {
    val positions = ListBuffer[Int](startPosition)
    for (roll <- rolls) {
      val position = positions.last + roll
      val wrappedPosition = if (position % 10 == 0) 10 else position % 10
      positions.addOne(wrappedPosition)
    }
    val scoringPositions = positions.tail // first roll doesn't give points
    lazy val playerPositions: LazyList[Int] = LazyList.from(scoringPositions) #::: playerPositions
    playerPositions
  }

  private def playGame = {
    p1Positions.zip(p2Positions).scanLeft((0, 0)) {
      case ((p1Score, p2Score), (p1Position, p2Position)) => calculateScores(p1Score, p1Position, p2Score, p2Position)
    }
      .zipWithIndex
      .find {
        case ((p1Score, p2Score), _) => hasGameFinished(p1Score, p2Score)
      }
      .map {
        case (scores@(p1Score, _), turnIndex) =>
          val rollsCount = calculateRollsCount(p1Score, turnIndex)
          (scores, rollsCount)
      }
      .get
  }

  private def calculateScores(p1Score: Int, p1Position: Int, p2Score: Int, p2Position: Int) = {
    val newP1Score = p1Score + p1Position
    if (newP1Score >= 1000)
      (newP1Score, p2Score)
    else
      (newP1Score, p2Score + p2Position)
  }

  private def hasGameFinished(p1Score: Int, p2Score: Int) =
    p1Score >= 1000 || p2Score >= 1000

  private def calculateRollsCount(p1Score: Int, turnIndex: Int) = {
    val hasP1Won = p1Score >= 1000
    if (hasP1Won)
      (turnIndex * 2 - 1) * 3
    else
      turnIndex * 2 * 3
  }
}
