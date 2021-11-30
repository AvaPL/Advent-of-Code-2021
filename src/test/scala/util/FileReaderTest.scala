package util

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.io.FileNotFoundException
import scala.util.{Failure, Success}

class FileReaderTest extends AnyWordSpec with Matchers {

  "read" when {
    "given filename of an existing file" should {
      "read the file" in {
        val result = FileReader.read("input/test_lines.txt")

        result should matchPattern {
          case Success(string: String) if string.nonEmpty =>
        }
      }
    }

    "given filename of an non-existing file" should {
      "return a failure" in {
        val result = FileReader.read("input/does_not_exist.txt")

        result shouldBe a[Failure[String]]
      }
    }
  }

  "readUnsafe" when {
    "given filename of an existing file" should {
      "read the file" in {
        val result = FileReader.readUnsafe("input/test_lines.txt")

        result should not be empty
      }
    }

    "given filename of an non-existing file" should {
      "return a failure" in {
        lazy val result = FileReader.readUnsafe("input/does_not_exist.txt")

        an[FileNotFoundException] shouldBe thrownBy(result)
      }
    }
  }
}
