package day7

import util.{FileReader, InputParser}
import util.InputParser._

object Puzzle2InputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitBy(",").map(_.toInt)
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day7/puzzle2.txt")
  val positions = Puzzle2InputParser.parse(input)
  val positionToCountMap = positions.groupBy(identity).view.mapValues(_.length)
  val (minPosition, maxPosition) = positionToCountMap.keys.foldLeft((Int.MaxValue, Int.MinValue)) {
    case ((min, max), position) => (position.min(min), position.max(max))
  }
  val costs = LazyList.range(minPosition, maxPosition + 1).map { finalPosition =>
    positionToCountMap.map {
      case (position, count) =>
        val distance = math.abs(finalPosition - position)
        distance * (distance + 1) / 2 * count
    }.sum
  }
  println(costs.min)
}
