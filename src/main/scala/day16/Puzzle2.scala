package day16

import util.{FileReader, InputParser}

object Puzzle2InputParser extends InputParser[String] {
  override def parse(string: String): String = {
    val binary = BigInt(string, 16).toString(2)
    val padding = "0" * (string.length * 4 - binary.length)
    padding + binary
  }
}

object Puzzle2 extends App with BitsParser {
  val input = FileReader.readUnsafe("input/day16/puzzle2.txt")
  val bits = Puzzle2InputParser.parse(input)
  val parsedPacket = parse(packet, bits).get
  val packetValue = evaluatePacketValue(parsedPacket)
  println(packetValue)

  private def evaluatePacketValue(packet: Packet): Long =
    packet match {
      case Literal(_, number) => number
      case Operator(_, packetType: OperatorPacket, subpackets) =>
        val values = subpackets.map(evaluatePacketValue)
        packetType match {
          case SumPacket => values.sum
          case ProductPacket => values.product
          case MinimumPacket => values.min
          case MaximumPacket => values.max
          case LessThanPacket => if (values(0) < values(1)) 1 else 0
          case GreaterThanPacket => if (values(0) > values(1)) 1 else 0
          case EqualToPacket => if (values(0) == values(1)) 1 else 0
        }
    }
}
