package day3

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day3/puzzle2.txt")
  val numbers = Puzzle2InputParser.parse(input)
  ???
}
