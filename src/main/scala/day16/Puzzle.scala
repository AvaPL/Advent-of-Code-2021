package day16

import util.{FileReader, InputParser}

object PuzzleInputParser extends InputParser[String] {
  override def parse(string: String): String =
    BigInt(string, 16).toString(2)
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day16/puzzle.txt")
  val bits = PuzzleInputParser.parse(input)
}
