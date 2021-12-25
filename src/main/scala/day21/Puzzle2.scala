package day21

import day21.Game.winningScore

import scala.collection.mutable

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

case class Game(
                 p1: Player,
                 p2: Player,
                 currentTurn: Turn = Player1Turn,
                 private val rollsCount: Int = 0,
                 private val dieScore: Int = 0
               ) {

  def rollDie(rolledScore: Int): Game = {
    val newRollsCount = rollsCount + 1
    val newDieScore = dieScore + rolledScore
    if (newRollsCount < 3)
      copy(rollsCount = newRollsCount, dieScore = newDieScore)
    else
      currentTurn match {
        case Player1Turn => movePlayer1(newDieScore)
        case Player2Turn => movePlayer2(newDieScore)
      }
  }

  private def movePlayer1(newDieScore: Int) = {
    val position = calculatePosition(p1.position, newDieScore)
    copy(
      p1 = p1.moveTo(position),
      currentTurn = currentTurn.next,
      rollsCount = 0,
      dieScore = 0
    )
  }

  private def calculatePosition(position: Int, dieScore: Int) = {
    val newPosition = position + dieScore
    if (newPosition % 10 == 0)
      10
    else
      newPosition % 10
  }

  private def movePlayer2(newDieScore: Int) = {
    val position = calculatePosition(p2.position, newDieScore)
    copy(
      p2 = p2.moveTo(position),
      currentTurn = currentTurn.next,
      rollsCount = 0,
      dieScore = 0
    )
  }

  def hasGameFinished: Boolean =
    p1.score >= winningScore || p2.score >= winningScore
}

object Game {
  val winningScore = 21
}

object Puzzle2 extends App {
  val p1StartPosition = 3
  val p2StartPosition = 7
  val dieResults = 1 to 3
  val game = Game(Player(p1StartPosition), Player(p2StartPosition))
  val (p1Wins, p2Wins) = countWinningUniverses(game)
  val mostWins = p1Wins.max(p2Wins)
  println(mostWins)

  private def countWinningUniverses(game: Game) = {
    val cache = mutable.Map[Game, (Long, Long)]()

    def loop(game: Game): (Long, Long) = {
      if (game.hasGameFinished)
        game.currentTurn match {
          case Player1Turn => (0, 1)
          case Player2Turn => (1, 0)
        }
      else {
        cache.getOrElseUpdate(game, {
          val (p1Wins, p2Wins) = dieResults.map(game.rollDie).map(loop).unzip
          (p1Wins.sum, p2Wins.sum)
        })
      }
    }

    loop(game)
  }
}
