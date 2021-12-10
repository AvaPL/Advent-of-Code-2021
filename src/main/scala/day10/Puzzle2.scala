package day10

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

object Puzzle2InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day10/puzzle2.txt")
  val lines = Puzzle2InputParser.parse(input)
  val bracesMap = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')
  val scoresMap = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
  val scores = (for (braces <- lines) yield bracesScore(braces)).flatten
  val middleScore = scores.sorted.apply(scores.length / 2)
  println(middleScore)

  private def bracesScore(braces: String) = {
    val bracesStack = mutable.Stack[Char]()
    var foundSyntaxError = false
    for (brace <- braces if !foundSyntaxError) {
      if (bracesMap.contains(brace))
        bracesStack.push(bracesMap(brace))
      else if (bracesStack.nonEmpty && bracesStack.top == brace)
        bracesStack.pop()
      else
        foundSyntaxError = true
    }
    Option.when(!foundSyntaxError) {
      bracesStack.foldLeft(0L) {
        case (totalScore, brace) => totalScore * 5 + scoresMap(brace)
      }
    }
  }
}
