package day7

import util.{FileReader, InputParser}
import util.InputParser._

import scala.math.abs

object Puzzle1InputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitBy(",").map(_.toInt)
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day7/puzzle1.txt")
  val positions = Puzzle1InputParser.parse(input)
  val (minPosition, maxPosition) = positions.foldLeft((Int.MaxValue, Int.MinValue)) {
    case ((min, max), position) => (position.min(min), position.max(max))
  }
  val positionToCountMap = positions.groupBy(identity).view.mapValues(_.length)
  val minCost = (minPosition to maxPosition).map(totalFuelCost).min
  println(minCost)

  private def totalFuelCost(finalPosition: Int) =
    positionToCountMap.map {
      case (position, count) => fuelCost(position, finalPosition) * count
    }.sum

  private def fuelCost(position: Int, finalPosition: Int) =
    abs(finalPosition - position)
}
