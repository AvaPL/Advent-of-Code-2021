package day21

import day21.Game.winningScore

case class Player(position: Int, score: Int = 0) {
  def moveTo(newPosition: Int): Player =
    copy(position = newPosition, score = score + newPosition)
}

sealed trait Turn {
  def next: Turn
}

case object Player1Turn extends Turn {
  override def next: Turn = Player2Turn
}

case object Player2Turn extends Turn {
  override def next: Turn = Player1Turn
}

case class Game(p1: Player, p2: Player, currentTurn: Turn = Player1Turn) {

  def rollDie(dieScore: Int): Game =
    currentTurn match {
      case Player1Turn =>
        val position = calculatePosition(p1.position, dieScore)
        copy(p1 = p1.moveTo(position), currentTurn = currentTurn.next)
      case Player2Turn =>
        val position = calculatePosition(p2.position, dieScore)
        copy(p2 = p2.moveTo(position), currentTurn = currentTurn.next)
    }

  private def calculatePosition(position: Int, dieScore: Int) = {
    val newPosition = position + dieScore
    if (newPosition % 10 == 0)
      10
    else
      newPosition % 10
  }

  def hasGameFinished: Boolean =
    p1.score >= winningScore || p2.score >= winningScore
}

object Game {
  val winningScore = 21
}

object Puzzle2 extends App {
  val p1StartPosition = 4 // TODO: Replace with input
  val p2StartPosition = 8 // TODO: Replace with input
  val dieResults = List(1, 2, 3)
  val game = Game(Player(p1StartPosition), Player(p2StartPosition))
  val (p1Wins, p2Wins) = countWinningUniverses(game)
  println(s"p1Wins = $p1Wins")
  println(s"p2Wins = $p2Wins")

  private def countWinningUniverses(game: Game, p1Wins: Long = 0, p2Wins: Long = 0): (Long, Long) = {
    if (game.hasGameFinished)
      game.currentTurn match {
        case Player1Turn => (p1Wins, p2Wins + 1)
        case Player2Turn => (p1Wins + 1, p2Wins)
      }
    else {
      // TODO: Roll die 3 times per turn
      val (allP1Wins, allP2Wins) = dieResults.map(game.rollDie).map(countWinningUniverses(_, p1Wins, p2Wins)).unzip
      (allP1Wins.sum, allP2Wins.sum)
    }
  }
}
