package day13

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[(Set[(Int, Int)], Seq[(Axis, Int)])] {
  override def parse(string: String): (Set[(Int, Int)], Seq[(Axis, Int)]) =
    string.splitBlocks match {
      case Seq(dotsRaw, foldsRaw) =>
        val dots = parseDots(dotsRaw)
        val folds = parseFolds(foldsRaw)
        (dots, folds)
    }

  private def parseDots(dotsRaw: String) =
    dotsRaw
      .splitLines
      .map(_.splitBy(","))
      .map {
        case Seq(x, y) => (x.toInt, y.toInt)
      }
      .toSet

  private def parseFolds(foldsRaw: String) =
    foldsRaw
      .splitLines
      .map(_.stripPrefix("fold along "))
      .map(_.splitBy("="))
      .map {
        case Seq(axisRaw, position) =>
          val axis = axisRaw match {
            case "x" => X
            case "y" => Y
          }
          (axis, position.toInt)
      }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day13/puzzle2.txt")
  val (dots, folds) = Puzzle2InputParser.parse(input)
  val afterFolds = folds.foldLeft(dots) {
    case (dots, (axis, axisPosition)) => dots.map {
      case (x, y) => axis match {
        case X => (fold(x, axisPosition), y)
        case Y => (x, fold(y, axisPosition))
      }
    }
  }
  val (maxX, maxY) = afterFolds.foldLeft((Int.MinValue, Int.MinValue)) {
    case ((maxX, maxY), (x, y)) => (maxX.max(x), maxY.max(y))
  }
  for (y <- 0 to maxY) {
    for (x <- 0 to maxX) {
      val char = if (afterFolds.contains((x, y))) '#' else '.'
      print(char)
    }
    println()
  }

  private def fold(coordinate: Int, axisPosition: Int) =
    if (coordinate > axisPosition) 2 * axisPosition - coordinate else coordinate
}
