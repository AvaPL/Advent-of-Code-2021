package util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InputParserTest extends AnyWordSpec with Matchers {

  "lines" when {
    "given string delimited by newlines" should {
      "return all lines" in {
        val input =
          """first
            |second
            |third
            |""".stripMargin

        val result = InputParser.lines(input)

        result should be(Seq("first", "second", "third"))
      }
    }
  }

  "blocks" when {
    "given string delimited by two newlines" should {
      "return all blocks" in {
        val input =
          """first
            |block
            |
            |second
            |block
            |""".stripMargin

        val result = InputParser.blocks(input)

        result should have length 2
      }
    }
  }

  "split" when {
    "given a string and . delimiter" should {
      "split it by delimiter" in {
        val input = "11.22.33"

        val result = InputParser.split(input, ".")

        result should be(Seq("11", "22", "33"))
      }
    }
  }

  "splitRegex" when {
    "given a string and [,.] regex" should {
      "split it by regex" in {
        val input = "1,2.3"

        val result = InputParser.splitRegex(input, "[,.]")

        result should be(Seq("1", "2", "3"))
      }
    }
  }
}
