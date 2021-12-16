package day16

import day16.BitsParser.BitsStringOps

import scala.util.parsing.combinator._


sealed trait Packet

case object Padding


case class PacketVersion(version: Int)

case class PacketType(id: Int)

case class PacketHeader(version: PacketVersion, packetType: PacketType)


case class LiteralNumberGroup(hexValue: Char)

case class LastLiteralNumberGroup(hexValue: Char)

case class LiteralNumber(value: Int)

case class Literal(header: PacketHeader, number: LiteralNumber) extends Packet


case class OperatorLengthTypeId(id: Int)

case class OperatorPacketHeader(packetHeader: PacketHeader, lengthTypeId: OperatorLengthTypeId)

case class OperatorSubpackets(subpackets: List[Packet])

case class Operator(header: OperatorPacketHeader, subpackets: OperatorSubpackets) extends Packet


trait BitsParser extends RegexParsers {

  def padding: Parser[Padding.type] =
    """0.+""".r ^^ { _ => Padding }


  def packetVersion: Parser[PacketVersion] =
    """[01]{3}""".r ^^ { version => PacketVersion(version.binToInt) }

  def packetType: Parser[PacketType] =
    """[01]{3}""".r ^^ { id => PacketType(id.binToInt) }

  def packetHeader: Parser[PacketHeader] =
    packetVersion ~ packetType ^^ { case version ~ packetType => PacketHeader(version, packetType) }


  def literalNumberGroup: Parser[LiteralNumberGroup] = {
    val pattern = """1([01]{4})""".r
    pattern ^^ { case pattern(hexValue) => LiteralNumberGroup(hexValue.binToSingleHex) }
  }

  def lastLiteralNumberGroup: Parser[LastLiteralNumberGroup] = {
    val pattern = """0([01]{4})""".r
    pattern ^^ { case pattern(hexValue) => LastLiteralNumberGroup(hexValue.binToSingleHex) }
  }

  def literalNumber: Parser[LiteralNumber] =
    literalNumberGroup.* ~ lastLiteralNumberGroup ^^ {
      case literalNumberGroups ~ lastLiteralNumberGroup =>
        val hexValue = s"${literalNumberGroups.map(_.hexValue).mkString}${lastLiteralNumberGroup.hexValue}"
        LiteralNumber(hexValue.hexToInt)
    }

  def literal: Parser[Literal] =
    packetHeader ~ literalNumber <~ padding.? ^^ { case header ~ number => Literal(header, number) }


  def operatorLengthTypeId: Parser[OperatorLengthTypeId] =
    """[01]""".r ^^ { id => OperatorLengthTypeId(id.binToInt) }

  def operatorPacketHeader: Parser[OperatorPacketHeader] =
    packetHeader ~ operatorLengthTypeId ^^ { case packetHeader ~ operatorLengthTypeId => OperatorPacketHeader(packetHeader, operatorLengthTypeId) }

  def operatorSubpackets: Parser[OperatorSubpackets] =
    (literal | operator).* ^^ { subpackets => OperatorSubpackets(subpackets) }

  def operator: Parser[Operator] =
    operatorPacketHeader ~ operatorSubpackets <~ padding.? ^^ { case header ~ subpackets => Operator(header, subpackets) }
}

object BitsParser {
  implicit class BitsStringOps(val string: String) extends AnyVal {
    def binToInt: Int = Integer.parseInt(string, 2)

    def binToSingleHex: Char = BigInt(string, 2).toString(16).head.toUpper

    def hexToInt: Int = Integer.parseInt(string, 16)
  }
}
