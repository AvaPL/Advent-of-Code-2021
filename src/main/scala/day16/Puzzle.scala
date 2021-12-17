package day16

import util.{FileReader, InputParser}

object PuzzleInputParser extends InputParser[String] {
  override def parse(string: String): String = {
    val binary = BigInt(string, 16).toString(2)
    val padding = "0" * (string.length * 4 - binary.length)
    padding + binary
  }
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day16/puzzle.txt")
  val bits = PuzzleInputParser.parse(input)
}
