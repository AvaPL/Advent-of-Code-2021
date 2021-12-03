package day3

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines.transpose.map(_.mkString)
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day3/puzzle1.txt")
  val columns = Puzzle1InputParser.parse(input)
  val gamma = Integer.parseInt(columns.map(_.groupBy(identity).view.mapValues(_.length).maxBy(_._2)._1).mkString, 2)
  val epsilon = ~gamma & 0xFFF
  println(gamma * epsilon)
}
