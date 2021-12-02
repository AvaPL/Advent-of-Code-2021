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
  val (horizontal, depth, _) = moveSequence.foldLeft((0, 0, 0)) {
    case ((horizontal, depth, aim), (direction, value)) => direction match {
      case "forward" => (horizontal + value, depth + value * aim, aim)
      case "up" => (horizontal, depth, aim - value)
      case "down" => (horizontal, depth, aim + value)
    }
  }
  println(horizontal * depth)
}
