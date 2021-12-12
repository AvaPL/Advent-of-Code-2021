package day12

import day12.Cave.isSmall
import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[Map[String, Set[String]]] {
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

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day12/puzzle2.txt")
  val caves = Puzzle2InputParser.parse(input)
  val paths = countPaths(startFrom = Cave.startName, visitedSmallCaves = Set.empty, visitedAdditionalSmallCave = false)
  println(paths)

  private def countPaths(startFrom: String, visitedSmallCaves: Set[String], visitedAdditionalSmallCave: Boolean): Int = {
    if (startFrom == Cave.endName) 1
    else {
      val visitedCavesPaths = countStandardPaths(startFrom, visitedSmallCaves, visitedAdditionalSmallCave)
      val revisitedCavesPaths = if (visitedAdditionalSmallCave) 0 else countPathsWithRevisits(startFrom, visitedSmallCaves)
      visitedCavesPaths + revisitedCavesPaths
    }
  }

  private def countStandardPaths(startFrom: String, visitedSmallCaves: Set[String], visitedAdditionalSmallCave: Boolean) = {
    val cavesToVisit = caves(startFrom).diff(visitedSmallCaves).toSeq
    val visitedCavesPaths = for {
      cave <- cavesToVisit
    } yield {
      val newVisitedSmallCaves = visitedSmallCaves ++ Option.when(isSmall(cave))(cave)
      countPaths(cave, newVisitedSmallCaves, visitedAdditionalSmallCave)
    }
    visitedCavesPaths.sum
  }

  private def countPathsWithRevisits(startFrom: String, visitedSmallCaves: Set[String]) = {
    val smallCavesToRevisit = caves(startFrom).intersect(visitedSmallCaves).toSeq
    val revisitedCavesPaths = for {
      cave <- smallCavesToRevisit
    } yield countPaths(cave, visitedSmallCaves, visitedAdditionalSmallCave = true)
    revisitedCavesPaths.sum
  }
}