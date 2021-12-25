package day22

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable

sealed trait State

case object On extends State

case object Off extends State

case class Command(state: State, x: Range, y: Range, z: Range) {
  lazy val cubes: Set[(Int, Int, Int)] =
    (for {
      i <- x
      j <- y
      k <- z
    } yield (i, j, k)).toSet
}

object Puzzle1InputParser extends InputParser[Seq[Command]] {
  override def parse(string: String): Seq[Command] =
    string.splitLines.map(parseCommand)

  private val commandRegex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r

  private def parseCommand(commandRaw: String) =
    commandRaw match {
      case commandRegex(state, x1, x2, y1, y2, z1, z2) =>
        Command(
          state = parseState(state),
          x = parseRange(x1, x2),
          y = parseRange(y1, y2),
          z = parseRange(z1, z2)
        )
    }

  private def parseState(stateRaw: String) =
    stateRaw match {
      case "on" => On
      case "off" => Off
    }

  private def parseRange(first: String, second: String) = {
    val firstInt = first.toInt
    val secondInt = second.toInt
    firstInt.min(secondInt) to firstInt.max(secondInt)
  }
}

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day22/puzzle1.txt")
  val rebootSteps = Puzzle1InputParser.parse(input)
  val cubes = mutable.Set[(Int, Int, Int)]()
  rebootSteps.foreach { step =>
    step.state match {
      case On => cubes.addAll(step.cubes)
      case Off => cubes.subtractAll(step.cubes)
    }
  }
  val onCubes = cubes.size
  println(onCubes)
}
