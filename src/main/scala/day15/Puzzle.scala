package day15

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

object PuzzleInputParser extends InputParser[Seq[Seq[Int]]] {
  override def parse(string: String): Seq[Seq[Int]] =
    for {
      line <- string.splitLines
    } yield for {
      riskLevel <- line.splitBy("")
    } yield riskLevel.toInt
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day15/puzzle.txt")
  val riskLevels = PuzzleInputParser.parse(input)
  val puzzle1 = solvePuzzle(riskLevels)
  println(s"puzzle1 = $puzzle1")
  val extendedRiskLevels = extendRiskLevels(riskLevels)
  val puzzle2 = solvePuzzle(extendedRiskLevels)
  println(s"puzzle2 = $puzzle2")

  private def solvePuzzle(riskLevels: Seq[Seq[Int]]) = {
    val upperBound = upperBoundHeuristic(riskLevels)
    dijkstra(riskLevels, upperBound)
  }

  // Calculates the upper bound by only going right and down
  private def upperBoundHeuristic(riskLevels: Seq[Seq[Int]]): Int = {
    val shortestPaths = Array.fill(riskLevels.length)(Array.fill(riskLevels.head.length)(Int.MaxValue))
    shortestPaths(0)(0) = 0
    for {
      i <- shortestPaths.indices
      j <- shortestPaths(i).indices
    } {
      val currentPathRisk = shortestPaths(i)(j)
      List((i, j + 1), (i + 1, j)).collect {
        case (i, j) if 0 <= i && 0 <= j && i < riskLevels.length && j < riskLevels.head.length =>
          val newRisk = currentPathRisk + riskLevels(i)(j)
          shortestPaths(i)(j) = shortestPaths(i)(j).min(newRisk)
      }
    }
    shortestPaths.last.last
  }

  // Uses various optimizations to avoid boxing issues and GC while being more or less readable
  // It is definitely not an efficient implementation of Dijkstra
  private def dijkstra(riskLevels: Seq[Seq[Int]], upperBound: Int) = {
    case class Position(totalRisk: Int, index: (Int, Int)) extends Ordered[Position] {
      override def compare(that: Position): Int = totalRisk.compareTo(that.totalRisk)
    }
    val shortestPaths = Array.fill(riskLevels.length)(Array.fill(riskLevels.head.length)(Int.MaxValue))
    val visitedPositions = mutable.PriorityQueue[Position]()
    visitedPositions.enqueue(Position(totalRisk = 0, index = (0, 0)))
    while (visitedPositions.nonEmpty) {
      val Position(currentPathRisk, (i, j)) = visitedPositions.dequeue
      if (currentPathRisk < upperBound)
        List((i, j + 1), (i, j - 1), (i - 1, j), (i + 1, j)).collect {
          case (i, j) if 0 <= i && 0 <= j && i < riskLevels.length && j < riskLevels.head.length =>
            val newRisk = currentPathRisk + riskLevels(i)(j)
            if (newRisk < shortestPaths(i)(j)) {
              shortestPaths(i)(j) = newRisk
              visitedPositions.enqueue(Position(newRisk, (i, j)))
            }
        }
    }
    shortestPaths.last.last
  }

  private def extendRiskLevels(riskLevels: Seq[Seq[Int]]) = {
    val extendedRows = riskLevels.map { row =>
      (0 to 4).flatMap(add => row.map(_ + add).map(clampRiskLevel))
    }
    (0 to 4).flatMap(add => extendedRows.map(_.map(_ + add).map(clampRiskLevel)))
  }

  private def clampRiskLevel(riskLevel: Int) =
    if (riskLevel >= 10) riskLevel - 9 else riskLevel
}
