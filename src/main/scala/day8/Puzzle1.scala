package day8

import util.{FileReader, InputParser}
import util.InputParser._

import scala.math.abs

object Puzzle1InputParser extends InputParser[Seq[Seq[String]]] {
  override def parse(string: String): Seq[Seq[String]] =
    string.splitLines.map(_.splitBy(" | ")(1).splitBy(" "))
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day8/puzzle1.txt")
  val digits = Puzzle1InputParser.parse(input)
  val uniqueSegmentsCount = Set(2, 4, 3, 7)
  val digits1478Count = digits.flatten.count(digit => uniqueSegmentsCount.contains(digit.length))
  println(digits1478Count)
}
