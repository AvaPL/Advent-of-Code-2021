package day24

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

// inp w
// mul x 0
// add x z
// mod x 26
// div z {DIV}
// add x {ADD_X}
// eql x w
// eql x 0
// mul y 0
// add y 25
// mul y x
// add y 1
// mul z y
// mul y 0
// add y w
// add y {ADD_Y}
// mul y x
// add z y
//
// Pattern repeats 14 times

case class Command(addX: Int, addY: Int)

object PuzzleInputParser extends InputParser[Seq[Command]] {
  override def parse(string: String): Seq[Command] =
    string.splitLines
      .grouped(18)
      .map {
        digitCommands =>
          val addX = digitCommands(5).stripPrefix("add x ").toInt
          val addY = digitCommands(15).stripPrefix("add y ").toInt
          Command(addX, addY)
      }
      .toSeq
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day24/puzzle.txt")
  val commands = PuzzleInputParser.parse(input)
  val stack = mutable.Stack[String]()
  commands.zipWithIndex.foreach {
    case (Command(addX, addY), index) =>
      if (addX > 0) {
        val rule = s"input[$index] + $addY"
        stack.push(rule)
      } else {
        val popped = stack.pop
        val rule = s"input[$index] = $popped $addX"
        println(rule)
      }
  }

  // input[5] = input[4] + 1
  // input[7] = input[6] - 3
  // input[8] = input[3] + 8
  // input[9] = input[2] - 7
  // input[11] = input[10] + 5
  // input[12] = input[1] + 3
  // input[13] = input[0] - 8
  //
  // Max:
  // input[0] = 9
  // input[1] = 6
  // input[2] = 9
  // input[3] = 1
  // input[4] = 8
  // input[5] = 9
  // input[6] = 9
  // input[7] = 6
  // input[8] = 9
  // input[9] = 2
  // input[10] = 4
  // input[11] = 9
  // input[12] = 9
  // input[13] = 1
  //
  // Min:
  // input[0] = 9
  // input[1] = 1
  // input[2] = 8
  // input[3] = 1
  // input[4] = 1
  // input[5] = 2
  // input[6] = 4
  // input[7] = 1
  // input[8] = 9
  // input[9] = 1
  // input[10] = 1
  // input[11] = 6
  // input[12] = 4
  // input[13] = 1
}
