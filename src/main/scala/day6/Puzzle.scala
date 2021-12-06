package day6

import util.InputParser._
import util.{FileReader, InputParser}

object PuzzleInputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitBy(",").map(_.toInt)
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day6/puzzle.txt")
  val lanternfish = PuzzleInputParser.parse(input)
  val puzzle1 = LanternfishCounter.count(lanternfish, 80)
  val puzzle2 = LanternfishCounter.count(lanternfish, 256)
  println(s"puzzle1 = $puzzle1")
  println(s"puzzle2 = $puzzle2")
}


