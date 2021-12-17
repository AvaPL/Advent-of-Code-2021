package day16

import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[String] {
  override def parse(string: String): String = {
    val binary = BigInt(string, 16).toString(2)
    val padding = "0" * (string.length * 4 - binary.length)
    padding + binary
  }
}

object Puzzle1 extends App with BitsParser {
  val input = FileReader.readUnsafe("input/day16/puzzle1.txt")
  val bits = Puzzle1InputParser.parse(input)
  val parsedPacket = parse(packet, bits).get
  val versionSum = sumVersions(parsedPacket)
  println(versionSum)

  private def sumVersions(packet: Packet): Int =
    packet match {
      case Literal(version, _) => version
      case Operator(version, _, subpackets) => version + subpackets.map(sumVersions).sum
    }
}
