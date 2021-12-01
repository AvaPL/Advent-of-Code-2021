package day1

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[Int]] {
  override def parse(string: String): Seq[Int] =
    string.splitLines.map(_.toInt)
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day1/puzzle1.txt")
  val measurements = Puzzle1InputParser.parse(input)
  val result = measurements.sliding(2).count {
    case Seq(x, y) if x < y => true
    case _ => false
  }
  println(result)
}
