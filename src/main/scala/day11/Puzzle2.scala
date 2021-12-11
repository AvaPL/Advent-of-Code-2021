package day11

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[Seq[Seq[OctopusEnergy]]] {
  override def parse(string: String): Seq[Seq[OctopusEnergy]] =
    for {
      row <- string.splitLines
    } yield for {
      value <- row.splitBy("")
    } yield OctopusEnergy(value.toInt)
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day11/puzzle2.txt")
  val octopuses = Puzzle2InputParser.parse(input)
  val octopusesCount = octopuses.map(_.length).sum
  val step = findSimultaneousFlashStep()
  println(step)

  private def findSimultaneousFlashStep(): Int =
    LazyList.from(1).find { step =>
      val flashes = countFlashes(step)
      flashes == octopusesCount
    }.get

  private def countFlashes(step: Int) =
    (for {
      i <- octopuses.indices
      j <- octopuses(i).indices
    } yield doStep(i, j, step)
      ).sum

  private def doStep(i: Int, j: Int, step: Int): Int = {
    val hasAlreadyFlashed = octopuses(i)(j).lastFlashStep.contains(step)
    var flashes = 0
    if (!hasAlreadyFlashed)
      octopuses(i)(j).value += 1
    if (octopuses(i)(j).value > 9)
      flashes += flash(i, j, step)
    flashes
  }

  private def flash(i: Int, j: Int, step: Int) = {
    octopuses(i)(j).value = 0
    octopuses(i)(j).lastFlashStep = Some(step)
    var flashes = 1
    for (k <- i - 1 to i + 1)
      for (l <- j - 1 to j + 1 if isIndexValid(k, l) && !(k == i && l == j))
        flashes += doStep(k, l, step)
    flashes
  }

  private def isIndexValid(i: Int, j: Int) =
    octopuses.lift(i).flatMap(_.lift(j)).isDefined
}
