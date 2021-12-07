package day7

import util.{FileReader, InputParser}
import util.InputParser._

import scala.math.abs

object Puzzle2InputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitBy(",").map(_.toInt)
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day7/puzzle2.txt")
  val positions = Puzzle2InputParser.parse(input)
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

  private def fuelCost(position: Int, finalPosition: Int) = {
    val distance = abs(finalPosition - position)
    distance * (distance + 1) / 2
  }
}
