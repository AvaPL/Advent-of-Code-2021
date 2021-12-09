package day9

import util.{FileReader, InputParser}
import util.InputParser._

import scala.math.abs

object Puzzle1InputParser extends InputParser[Seq[Seq[Int]]] {
  override def parse(string: String): Seq[Seq[Int]] =
    for {
      line <- string.splitLines
    } yield for {
      value <- line.splitBy("")
    } yield value.toInt
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day9/puzzle1.txt")
  val heightmap = Puzzle1InputParser.parse(input)
  var lowPointsSum = 0
  for (i <- heightmap.indices) {
    for (j <- heightmap(i).indices) {
      val value = heightmap(i)(j)
      val up = heightmap(i).lift(j + 1)
      val down = heightmap(i).lift(j - 1)
      val left = heightmap.lift(i - 1).map(_ (j))
      val right = heightmap.lift(i + 1).map(_ (j))
      val isLowPoint = List(up, down, left, right).flatten.forall(_ > value)
      if (isLowPoint)
        lowPointsSum += value + 1
    }
  }
  println(lowPointsSum)
}
