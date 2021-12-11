package day11

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[Seq[OctopusEnergy]]] {
  override def parse(string: String): Seq[Seq[OctopusEnergy]] =
    for {
      row <- string.splitLines
    } yield for {
      value <- row.splitBy("")
    } yield OctopusEnergy(value.toInt)
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day11/puzzle1.txt")
  val octopuses = Puzzle1InputParser.parse(input)
  var flashes = 0
  doSteps()
  println(flashes)

  private def doSteps(): Unit =
    for (step <- 1 to 100)
      for (i <- octopuses.indices)
        for (j <- octopuses(i).indices)
          doStep(i, j, step)

  private def doStep(i: Int, j: Int, step: Int): Unit = {
    val hasAlreadyFlashed = octopuses(i)(j).lastFlashStep.contains(step)
    if (!hasAlreadyFlashed)
      octopuses(i)(j).value += 1
    if (octopuses(i)(j).value > 9)
      flash(i, j, step)
  }

  private def flash(i: Int, j: Int, step: Int): Unit = {
    octopuses(i)(j).value = 0
    octopuses(i)(j).lastFlashStep = Some(step)
    flashes += 1
    for (k <- i - 1 to i + 1)
      for (l <- j - 1 to j + 1 if isIndexValid(k, l) && !(k == i && l == j))
        doStep(k, l, step)
  }

  private def isIndexValid(i: Int, j: Int): Boolean =
    octopuses.lift(i).flatMap(_.lift(j)).isDefined
}
