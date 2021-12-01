package day1

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitLines.map(_.toInt)
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day1/puzzle2.txt")
  val measurements = Puzzle2InputParser.parse(input)
  val result = measurements.sliding(3).map(_.sum).sliding(2).count {
    case Seq(x, y) => x < y
  }
  println(result)
}
