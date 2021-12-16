package day16

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BitsParserTest extends AnyWordSpec with Matchers with BitsParser {

  "packetHeader" when {
    "given version 4 (100) and type 4 (100)" should {
      "parse" in {
        val input = "100100"

        val parsedPacketHeader = parse(packetHeader, input)

        val expected = PacketHeader(PacketVersion(4), PacketType(4))
        parsedPacketHeader.get should be(expected)
      }
    }
  }

  "literalNumber" when {
    "given 2021 (011111100101, encoded as 101111111000101)" should {
      "parse" in {
        val input = "101111111000101"

        val parsedLiteralNumber = parse(literalNumber, input)

        val expected = LiteralNumber(2021)
        parsedLiteralNumber.get should be(expected)
      }
    }
  }

  // TODO: Test example inputs
}
