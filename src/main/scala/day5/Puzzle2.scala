package day5

import util.{FileReader, InputParser}
import util.InputParser._

object Puzzle2InputParser extends InputParser[Seq[Line]] {
  override def parse(string: String): Seq[Line] =
    string.splitLines.map(_.splitBy(" -> ")).map {
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
  val lines = Puzzle1InputParser.parse(input)
  val ventsMap = VentsMap(1000)
  lines.flatMap(_.coveredPoints).foreach(ventsMap.markPoint)
  println(ventsMap.countDangerousAreas)
}
