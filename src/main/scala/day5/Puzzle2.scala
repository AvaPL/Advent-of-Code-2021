package day5

import util.{FileReader, InputParser}
import util.InputParser._

object Puzzle2InputParser extends InputParser[Seq[Line]] {
  override def parse(string: String): Seq[Line] =
    string.splitLines.map(parseLine)

  private def parseLine(lineRaw: String) =
    lineRaw.splitBy(" -> ") match {
      case Seq(startRaw, endRaw) =>
        val start = parsePoint(startRaw)
        val end = parsePoint(endRaw)
        Line(start, end)
    }

  private def parsePoint(pointRaw: String) =
    pointRaw.splitBy(",") match {
      case Seq(x, y) => Point(x.toInt, y.toInt)
    }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day5/puzzle2.txt")
  val lines = Puzzle2InputParser.parse(input)
  val ventsMap = lines.foldLeft(VentsMap(1000))(_.markLine(_))
  println(ventsMap.countDangerousAreas)
}
