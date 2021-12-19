package day18

import util.{FileReader, InputParser}

import scala.util.parsing.combinator.RegexParsers
import InputParser._

sealed trait SnailfishNumber

case class RegularNumber(value: Int) extends SnailfishNumber

case class NumberPair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber


object Puzzle1InputParser extends InputParser[Seq[SnailfishNumber]] with RegexParsers {
  override def parse(string: String): Seq[SnailfishNumber] =
    string.splitLines.map(parseAll(snailfishNumber, _).get)

  private def snailfishNumber =
    regularNumber | numberPair

  private def regularNumber =
    """\d""".r ^^ { digit => RegularNumber(digit.toInt) }

  private def numberPair: Parser[NumberPair] =
    ("[" ~> snailfishNumber <~ ",") ~ (snailfishNumber <~ "]") ^^ { case left ~ right => NumberPair(left, right) }
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day18/puzzle1.txt")
  val numbers = Puzzle1InputParser.parse(input)
  println(numbers)
}
