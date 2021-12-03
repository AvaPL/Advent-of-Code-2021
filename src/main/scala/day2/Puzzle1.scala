package day2

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[(String, Int)]] {
  override def parse(string: String): Seq[(String, Int)] =
    string.splitLines.map(_.splitBy(" ")).collect {
      case Seq(direction, value) => (direction, value.toInt)
    }
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day2/puzzle1.txt")
  val moveSequence = Puzzle1InputParser.parse(input)
  val (horizontal, depth) = moveSequence.foldLeft((0, 0)) {
    case ((horizontal, depth), (direction, value)) => direction match {
      case "forward" => (horizontal + value, depth)
      case "up" => (horizontal, depth - value)
      case "down" => (horizontal, depth + value)
    }
  }
  println(horizontal * depth)
}
