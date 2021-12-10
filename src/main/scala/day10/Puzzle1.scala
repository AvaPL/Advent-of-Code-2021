package day10

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

object Puzzle1InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day10/puzzle1.txt")
  val lines = Puzzle1InputParser.parse(input)
  val bracesMap = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val scoresMap = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  var score = 0
  for (braces <- lines) {
    val bracesStack = mutable.Stack[Char]()
    var foundSyntaxError = false
    for (brace <- braces if !foundSyntaxError) {
      if (bracesMap.contains(brace))
        bracesStack.push(bracesMap(brace))
      else if (bracesStack.nonEmpty && bracesStack.top == brace)
        bracesStack.pop()
      else {
        foundSyntaxError = true
        score += scoresMap(brace)
      }
    }
  }
  println(score)
}
