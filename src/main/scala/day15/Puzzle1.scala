package day15

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

object Puzzle1InputParser extends InputParser[Seq[Seq[Int]]] {
  override def parse(string: String): Seq[Seq[Int]] =
    for {
      line <- string.splitLines
    } yield for {
      riskLevel <- line.splitBy("")
    } yield riskLevel.toInt
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day15/puzzle1.txt")
  val riskLevels = Puzzle1InputParser.parse(input)
  val shortestPaths = dijkstra(riskLevels)
  val bottomRightCost = shortestPaths.last.last
  println(bottomRightCost)

  // Uses various optimizations to avoid boxing issues and GC while being more or less readable
  private def dijkstra(riskLevels: Seq[Seq[Int]]): Seq[Seq[Int]] = {
    case class Position(totalRisk: Int, index: (Int, Int)) extends Ordered[Position] {
      override def compare(that: Position): Int = totalRisk.compareTo(that.totalRisk)
    }
    val shortestPaths = Array.fill(riskLevels.length)(Array.fill(riskLevels.head.length)(Int.MaxValue))
    val visitedPositions = mutable.PriorityQueue[Position]()
    visitedPositions.enqueue(Position(totalRisk = 0, index = (0, 0)))
    while (visitedPositions.nonEmpty) {
      val Position(currentPathRisk, (i, j)) = visitedPositions.dequeue
      List((i, j + 1), (i, j - 1), (i - 1, j), (i + 1, j)).collect {
        case (i, j) if 0 <= i && 0 <= j && i < riskLevels.length && j < riskLevels.head.length =>
          val newRisk = currentPathRisk + riskLevels(i)(j)
          if (newRisk < shortestPaths(i)(j)) {
            shortestPaths(i)(j) = newRisk
            visitedPositions.enqueue(Position(newRisk, (i, j)))
          }
      }
    }
    shortestPaths.map(_.toSeq).toSeq
  }
}
