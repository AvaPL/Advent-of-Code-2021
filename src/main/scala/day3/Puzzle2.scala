package day3

import util.InputParser._
import util.{FileReader, InputParser}

import scala.annotation.tailrec

object Puzzle2InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines
}

sealed trait RatingType

case object Oxygen extends RatingType

case object CO2 extends RatingType

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day3/puzzle2.txt")
  val numbers = Puzzle2InputParser.parse(input)
  val oxygen = Integer.parseInt(rating(numbers, 0, Oxygen), 2)
  val co2 = Integer.parseInt(rating(numbers, 0, CO2), 2)
  println(oxygen * co2)

  @tailrec
  def rating(numbers: Seq[String], index: Int, ratingType: RatingType): String =
    numbers match {
      case Seq(rating) => rating
      case _ =>
        val filtered = filterByFrequency(numbers, index, ratingType)
        rating(filtered, index + 1, ratingType)
    }

  private def filterByFrequency(numbers: Seq[String], index: Int, ratingType: RatingType) = {
    val frequencyMap = numbers.map(_ (index)).groupBy(identity).view.mapValues(_.size)
    val zeros = frequencyMap.getOrElse('0', 0)
    val ones = frequencyMap.getOrElse('1', 0)
    val filterOnChar = ratingType match {
      case Oxygen => if (zeros > ones) '0' else '1'
      case CO2 => if (ones < zeros) '1' else '0'
    }
    numbers.filter(_ (index) == filterOnChar)
  }
}
