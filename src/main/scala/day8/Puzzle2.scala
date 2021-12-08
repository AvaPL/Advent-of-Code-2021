package day8

import util.{FileReader, InputParser}
import util.InputParser._

import scala.collection.immutable.{AbstractSet, SortedSet}
import scala.math.abs

object Puzzle2InputParser extends InputParser[Seq[(Seq[String], Seq[String])]] {
  override def parse(string: String): Seq[(Seq[String], Seq[String])] =
    (for {
      line <- string.splitLines
    } yield for {
      parts <- line.splitBy(" | ")
    } yield for {
      digits <- parts.splitBy(" ")
    } yield digits).map {
      case Seq(left, right) => (left, right)
    }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day8/puzzle2.txt")
  val digits = Puzzle2InputParser.parse(input)
  val decodedNumbers = digits.map {
    case (digits, encodedValue) => decodeValue(digits, encodedValue)
  }
  println(decodedNumbers.sum)

  private def decodeValue(digits: Seq[String], encodedValue: Seq[String]) = {
    val digit1 = digits.find(_.length == 2).get.toSet
    val digit4 = digits.find(_.length == 4).get.toSet
    val decodedDigits = encodedValue.map(_.toSet).map { digit =>
      digit.size match {
        case 2 => 1
        case 3 => 7
        case 4 => 4
        case 5 if digit.intersect(digit1).size == 2 => 3
        case 5 if digit.intersect(digit4).size == 2 => 2
        case 5 => 5
        case 6 if digit.intersect(digit1).size == 1 => 6
        case 6 if digit.intersect(digit4).size == 4 => 9
        case 6 => 0
        case 7 => 8
      }
    }
    decodedDigits.mkString.toInt
  }
}
