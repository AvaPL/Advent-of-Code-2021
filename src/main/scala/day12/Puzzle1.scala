package day12

import day12.Cave.isSmall
import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Map[String, Set[String]]] {
  override def parse(string: String): Map[String, Set[String]] =
    string.splitLines
      .map(_.splitBy("-"))
      .flatMap {
        case Seq(from, to) => Seq((from, to), (to, from))
      }
      .groupBy(_._1)
      .filterNot(_._1 == Cave.endName) // connections from end do not matter
      .view.mapValues {
      _.map(_._2)
        .filterNot(_ == Cave.startName) // connections to start do not matter
        .toSet
    }
      .toMap
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day12/puzzle1.txt")
  val caves = Puzzle1InputParser.parse(input)
  val paths = countPathsToEnd(Cave.startName)
  println(paths)

  private def countPathsToEnd(startFrom: String, visitedSmallCaves: Set[String] = Set.empty): Int = {
    if (startFrom == Cave.endName) 1
    else {
      val cavesToVisit = caves(startFrom).diff(visitedSmallCaves).toSeq
      val visitedCavesPaths = for {
        cave <- cavesToVisit
      } yield {
        val newVisitedSmallCaves = visitedSmallCaves ++ Option.when(isSmall(cave))(cave)
        countPathsToEnd(cave, newVisitedSmallCaves)
      }
      visitedCavesPaths.sum
    }
  }
}
