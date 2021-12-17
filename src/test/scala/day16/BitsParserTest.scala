package day16

import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BitsParserTest extends AnyWordSpec with Matchers with Inspectors with BitsParser {

  "packetHeader" when {
    "given version 4 (100) and type 4 (100)" should {
      "parse" in {
        val input = "100100"

        val parsedPacketHeader = parseAll(packetHeader, input)

        val expected = PacketHeader(PacketVersion(4), PacketType(4))
        parsedPacketHeader.get should be(expected)
      }
    }
  }

  "literalNumber" when {
    "given 2021 (011111100101, encoded as 101111111000101)" should {
      "parse" in {
        val input = "101111111000101"

        val parsedLiteralNumber = parseAll(literalNumber, input)

        val expected = LiteralNumber(2021)
        parsedLiteralNumber.get should be(expected)
      }
    }
  }

  "literal" when {
    "given a valid literal" should {
      "parse" in {
        val input = "110100101111111000101000"

        val parsedLiteral = parse(packet, input)

        val expected = Literal(PacketHeader(PacketVersion(6), PacketType(4)), LiteralNumber(2021))
        parsedLiteral.get should be(expected)
      }
    }
  }

  "operator" should {
    "parse" when {
      "given a valid operator with 2 packets" in {
        val input = "00111000000000000110111101000101001010010001001000000000"

        val parsedOperator = parse(packet, input)

        val expectedHeader = OperatorPacketHeader(PacketHeader(PacketVersion(1), PacketType(6)), OperatorTotalLength(27))
        val expectedFirstPacket = Literal(PacketHeader(PacketVersion(6), PacketType(4)), LiteralNumber(10))
        val expectedSecondPacket = Literal(PacketHeader(PacketVersion(2), PacketType(4)), LiteralNumber(20))
        val expected = Operator(expectedHeader, OperatorSubpackets(List(expectedFirstPacket, expectedSecondPacket)))
        parsedOperator.get should be(expected)
      }

      "given a valid operator with 3 packets" in {
        val input = "11101110000000001101010000001100100000100011000001100000"

        val parsedOperator = parse(packet, input)

        val expectedHeader = OperatorPacketHeader(PacketHeader(PacketVersion(7), PacketType(3)), OperatorSubpacketsCount(3))
        val expectedFirstPacket = Literal(PacketHeader(PacketVersion(2), PacketType(4)), LiteralNumber(1))
        val expectedSecondPacket = Literal(PacketHeader(PacketVersion(4), PacketType(4)), LiteralNumber(2))
        val expectedThirdPacket = Literal(PacketHeader(PacketVersion(1), PacketType(4)), LiteralNumber(3))
        val expected = Operator(expectedHeader, OperatorSubpackets(List(expectedFirstPacket, expectedSecondPacket, expectedThirdPacket)))
        parsedOperator.get should be(expected)
      }

      "given multiple valid operators" in {
        val inputs = List(
          "8A004A801A8002F478",
          "620080001611562C8802118E34",
          "C0015000016115A2E0802F182340",
          "A0016C880162017C3686B18A3D4780"
        ).map(PuzzleInputParser.parse)

        val parsingResults = inputs.map(parse(packet, _))

        forAll(parsingResults)(_.successful should be(true))
      }
    }
  }
}
