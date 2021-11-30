package util

import scala.io.Source
import scala.util.{Try, Using}

object FileReader {

  def readUnsafe(filename: String): String = read(filename).get

  def read(filename: String): Try[String] =
    Using(Source.fromResource(filename))(_.mkString)
}
