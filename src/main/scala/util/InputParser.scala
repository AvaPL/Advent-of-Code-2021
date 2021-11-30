package util

import scala.util.matching.Regex

trait InputParser[T] {
  def parse(string: String): T
}

object InputParser {
  def split(string: String, delimiter: String): Seq[String] = {
    val quotedDelimiter = Regex.quote(delimiter)
    string.split(quotedDelimiter).toSeq
  }

  def splitRegex(string: String, regex: String): Seq[String] =
    string.split(regex).toSeq

  def lines(string: String): Seq[String] =
    split(string, "\n")

  def blocks(string: String): Seq[String] =
    split(string, "\n\n")
}