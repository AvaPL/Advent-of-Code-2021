package util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import InputParser._

class InputParserTest extends AnyWordSpec with Matchers {

  "splitLines" when {
    "given string delimited by newlines" should {
      "return all lines" in {
        val input =
          """first
            |second
            |third
            |""".stripMargin

        val result = input.splitLines

        result should be(Seq("first", "second", "third"))
      }
    }
  }

  "splitBlocks" when {
    "given string delimited by two newlines" should {
      "return all blocks" in {
        val input =
          """first
            |block
            |
            |second
            |block
            |""".stripMargin

        val result = input.splitBlocks

        result should have length 2
      }
    }
  }

  "splitBy" when {
    "given a string and . delimiter" should {
      "split it by delimiter" in {
        val input = "11.22.33"

        val result = input.splitBy(".")

        result should be(Seq("11", "22", "33"))
      }
    }
  }

  "splitByRegex" when {
    "given a string and [,.] regex" should {
      "split it by regex" in {
        val input = "1,2.3"

        val result = input.splitByRegex("[,.]")

        result should be(Seq("1", "2", "3"))
      }
    }
  }
}
