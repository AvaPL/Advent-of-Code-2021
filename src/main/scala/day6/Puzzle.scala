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
  val puzzle1 = count(lanternfish, 80)
  val puzzle2 = count(lanternfish, 256)
  println(s"puzzle1 = $puzzle1")
  println(s"puzzle2 = $puzzle2")

  private def count(lanternfish: Seq[Int], days: Int): Long = {
    val countArray = lanternfishCountArray(lanternfish)
    for (_ <- 1 to days) {
      val newLanternfish = countArray(0)
      for (days <- 1 to 8)
        countArray(days - 1) = countArray(days)
      countArray(6) += newLanternfish
      countArray(8) = newLanternfish
    }
    countArray.sum
  }

  private def lanternfishCountArray(lanternfish: Seq[Int]) = {
    val array = Array.fill(9)(0L)
    lanternfish.groupBy(identity).foreach {
      case (i, value) => array(i) = value.length
    }
    array
  }
}


