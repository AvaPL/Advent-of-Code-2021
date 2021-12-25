package day25

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[String]] {
  override def parse(string: String): Seq[String] =
    string.splitLines
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day25/puzzle1.txt")
  val cucumbers = Puzzle1InputParser.parse(input)
  val stopStep = calculateStopStep(cucumbers)
  println(stopStep)

  private def calculateStopStep(initialCucumbers: Seq[String]) = {
    var steps = 0
    var cucumbers = initialCucumbers
    var hasMovementStopped = false
    while (!hasMovementStopped) {
      steps += 1
      val afterStep = makeStep(cucumbers)
      hasMovementStopped = afterStep == cucumbers
      cucumbers = afterStep
    }
    steps
  }

  private def makeStep(cucumbers: Seq[String]) = {
    val movedRight = moveCucumbersRight(cucumbers)
    moveCucumbersDown(movedRight)
  }

  private def moveCucumbersRight(cucumbers: Seq[String]): Seq[String] =
    moveCucumbersRight(cucumbers, cucumberChar = '>')

  private def moveCucumbersRight(cucumbers: Seq[String], cucumberChar: Char): Seq[String] = {
    cucumbers.map { row =>
      val moved = row.replaceAll(s"$cucumberChar\\.", s".$cucumberChar")
      if (row.head == '.' && row.last == cucumberChar)
        cucumberChar + moved.drop(1).dropRight(1) + '.'
      else
        moved
    }
  }

  private def moveCucumbersDown(cucumbers: Seq[String]) = {
    val transposed = transpose(cucumbers)
    val movedDown = moveCucumbersRight(transposed, cucumberChar = 'v')
    transpose(movedDown)
  }

  private def transpose(cucumbers: Seq[String]) =
    cucumbers.transpose.map(_.mkString)
}
