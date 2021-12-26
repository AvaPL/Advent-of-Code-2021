package day22

import util.InputParser._
import util.{FileReader, InputParser}

import scala.collection.mutable
import scala.math.{abs, max, min}

sealed trait State

case object On extends State

case object Off extends State

case class Point(x: Int, y: Int, z: Int)

case class Cuboid(topCorner: Point, bottomCorner: Point) {

  lazy val volume: Long =
    (abs(topCorner.x - bottomCorner.x) + 1).toLong *
      (abs(topCorner.y - bottomCorner.y) + 1) *
      (abs(topCorner.z - bottomCorner.z) + 1)

  def intersection(other: Cuboid): Option[Cuboid] = {
    val intersectionTopCorner = Point(max(topCorner.x, other.topCorner.x), max(topCorner.y, other.topCorner.y), min(topCorner.z, other.topCorner.z))
    val intersectionBottomCorner = Point(min(bottomCorner.x, other.bottomCorner.x), min(bottomCorner.y, other.bottomCorner.y), max(bottomCorner.z, other.bottomCorner.z))
    Option.when(isValidIntersection(intersectionTopCorner, intersectionBottomCorner))(Cuboid(intersectionTopCorner, intersectionBottomCorner))
  }

  private def isValidIntersection(topCorner: Point, bottomCorner: Point) =
    topCorner.x <= bottomCorner.x &&
      topCorner.y <= bottomCorner.y &&
      topCorner.z >= bottomCorner.z
}

case class Command(state: State, cuboid: Cuboid)

object PuzzleInputParser extends InputParser[Seq[Command]] {
  override def parse(string: String): Seq[Command] =
    string.splitLines.map(parseCommand)

  private val commandRegex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r

  private def parseCommand(commandRaw: String) =
    commandRaw match {
      case commandRegex(stateRaw, x1, x2, y1, y2, z1, z2) =>
        val state = parseState(stateRaw)
        val x = parseRange(x1, x2)
        val y = parseRange(y1, y2)
        val z = parseRange(z1, z2)
        val topCorner = Point(x._1, y._1, z._2)
        val bottomCorner = Point(x._2, y._2, z._1)
        Command(state, Cuboid(topCorner, bottomCorner))
    }

  private def parseState(stateRaw: String) =
    stateRaw match {
      case "on" => On
      case "off" => Off
    }

  private def parseRange(first: String, second: String) = {
    val firstInt = first.toInt
    val secondInt = second.toInt
    (firstInt.min(secondInt), firstInt.max(secondInt))
  }
}

object Puzzle extends App {
  val puzzle1 = solvePuzzle(puzzle = 1)
  println(s"puzzle1 = $puzzle1")
  val puzzle2 = solvePuzzle(puzzle = 2)
  println(s"puzzle2 = $puzzle2")

  private def solvePuzzle(puzzle: Int) = {
    val input = FileReader.readUnsafe(s"input/day22/puzzle$puzzle.txt")
    val rebootSteps = PuzzleInputParser.parse(input)
    val commands = effectiveCommands(rebootSteps)
    volumeFromCommands(commands)
  }

  private def effectiveCommands(rebootSteps: Seq[Command]) = {
    val commands = mutable.ListBuffer[Command]()
    for (step <- rebootSteps) {
      val intersections = getIntersections(commands, step)
      val intersectionsCommands = intersections.map(commandForIntersection(step.state, _))
      commands.appendAll(intersectionsCommands)
      if (step.state == On)
        commands.append(step)
    }
    commands.toSeq
  }

  private def getIntersections(commands: mutable.ListBuffer[Command], step: Command) =
    commands.flatMap { command =>
      val intersection = command.cuboid.intersection(step.cuboid)
      intersection.map(Command(command.state, _))
    }

  private def commandForIntersection(stepState: State, intersection: Command) =
    (stepState, intersection.state) match {
      case (Off, Off) => intersection.copy(state = On)
      case (Off, On) => intersection.copy(state = Off)
      case (On, Off) => intersection.copy(state = On)
      case (On, On) => intersection.copy(state = Off)
    }

  private def volumeFromCommands(commands: Seq[Command]) =
    commands.map { command =>
      command.state match {
        case On => command.cuboid.volume
        case Off => -command.cuboid.volume
      }
    }.sum
}
