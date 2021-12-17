package day16

import day16.BitsParser.BitsStringOps

import scala.util.parsing.combinator._


sealed trait Packet

case object Padding


sealed trait PacketType

case object LiteralPacket extends PacketType

case object OperatorPacket extends PacketType


case class Literal(version: Int, number: Long) extends Packet


sealed trait OperatorLength

case class OperatorTotalLength(bits: Int) extends OperatorLength

case class OperatorSubpacketsCount(count: Int) extends OperatorLength

case class Operator(version: Int, subpackets: List[Packet]) extends Packet


trait BitsParser extends RegexParsers {

  def packet: Parser[Packet] =
    literal | operator

  def padding: Parser[Padding.type] =
    """0.+""".r ^^ { _ => Padding }


  def literal: Parser[Literal] =
    (packetVersion <~ packetType.filter(_ == LiteralPacket)) ~ literalNumber ^^ {
      case version ~ number => Literal(version, number)
    }

  private[day16] def packetVersion: Parser[Int] =
    """[01]{3}""".r ^^ {
      _.binToInt
    }

  private[day16] def packetType: Parser[PacketType] =
    """[01]{3}""".r ^^ {
      _.binToInt
    } ^^ {
      case 4 => LiteralPacket
      case _ => OperatorPacket
    }

  private[day16] def literalNumber = {
    literalNumberGroup.* ~ lastLiteralNumberGroup ^^ {
      case literalNumberGroups ~ lastLiteralNumberGroup =>
        val hexValue = s"${literalNumberGroups.mkString}$lastLiteralNumberGroup"
        hexValue.hexToLong
    }
  }

  private def literalNumberGroup = {
    val pattern = """1([01]{4})""".r
    pattern ^^ { case pattern(hexValue) => hexValue.binToSingleHex }
  }

  private def lastLiteralNumberGroup = {
    val pattern = """0([01]{4})""".r
    pattern ^^ { case pattern(hexValue) => hexValue.binToSingleHex }
  }


  def operator: Parser[Operator] =
    (packetVersion <~ packetType.filter(_ == OperatorPacket)) ~ (operatorLength >> operatorSubpackets) ^^ {
      case version ~ subpackets => Operator(version, subpackets)
    }

  private def operatorLength: Parser[OperatorLength] =
    ("""[01]""".r ^^ {
      _.binToInt
    }) >> {
      case 0 => """[01]{15}""".r ^^ { bits => OperatorTotalLength(bits.binToInt) }
      case 1 => """[01]{11}""".r ^^ { count => OperatorSubpacketsCount(count.binToInt) }
    }

  private def operatorSubpackets(length: OperatorLength): Parser[List[Packet]] =
    length match {
      case OperatorSubpacketsCount(count) => repN(count, packet)
      case OperatorTotalLength(bits) => s"""[01]{$bits}""".r ^^ {
        parseAll(packet.*, _)
      } ^? {
        case Success(packets, _) => packets
      }
    }
}

object BitsParser {
  implicit class BitsStringOps(val string: String) extends AnyVal {
    def binToInt: Int = Integer.parseInt(string, 2)

    def binToSingleHex: Char = BigInt(string, 2).toString(16).head.toUpper

    def hexToLong: Long = java.lang.Long.parseLong(string, 16)
  }
}
