package day2

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[Seq[(String, Int)]] {
  override def parse(string: String): Seq[(String, Int)] =
    string.splitLines.map(_.splitBy(" ")).collect {
      case Seq(direction, value) => (direction, value.toInt)
    }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day2/puzzle2.txt")
  val moveSequence = Puzzle2InputParser.parse(input)
  var aim = 0
  val (horizontal, depth) = moveSequence.foldLeft((0, 0)) {
    case ((horizontal, depth), (direction, value)) => direction match {
      case "forward" => (horizontal + value, depth + value * aim)
      case "up" =>
        aim -= value
        (horizontal, depth)
      case "down" =>
        aim += value
        (horizontal, depth)
    }
  }
  println(horizontal * depth)
}
