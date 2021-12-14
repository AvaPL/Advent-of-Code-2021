package day14

import util.{FileReader, InputParser}
import util.InputParser._

object PuzzleInputParser extends InputParser[(String, Map[String, Char])] {
  override def parse(string: String): (String, Map[String, Char]) =
    string.splitBlocks match {
      case Seq(template, rulesRaw) =>
        val rules = parseRules(rulesRaw)
        (template, rules)
    }

  private def parseRules(rulesRaw: String) =
    rulesRaw
      .splitLines
      .map(_.splitBy(" -> "))
      .map {
        case Seq(pair, element) => (pair, element.head)
      }
      .toMap
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day14/puzzle.txt")
  val (template, rules) = PuzzleInputParser.parse(input)
  val puzzle1 = solvePuzzle(steps = 10)
  println(s"puzzle1 = $puzzle1")
  val puzzle2 = solvePuzzle(steps = 40)
  println(s"puzzle2 = $puzzle2")

  private def solvePuzzle(steps: Int) = {
    val charsCount = (1 to steps).foldLeft((initialTemplateElements(template), initialCharsCount(template))) {
      case ((afterStep, charsCount), _) => insertionStep(afterStep, charsCount)
    }._2
    val (minCount, maxCount) = minMaxCount(charsCount)
    maxCount - minCount
  }

  private def initialTemplateElements(template: String) =
    template
      .sliding(2)
      .toSeq
      .groupBy(identity)
      .view.mapValues(_.length.toLong).toMap

  private def initialCharsCount(template: String) =
    template
      .groupBy(identity)
      .view.mapValues(_.length.toLong).toMap

  private def insertionStep(templateElements: Map[String, Long], charsCount: Map[Char, Long]) =
    templateElements.toSeq.map {
      case (element, count) =>
        val ruleChar = rules(element)
        val leftElement = s"${element(0)}$ruleChar"
        val rightElement = s"$ruleChar${element(1)}"
        val newTemplateElements = Seq(leftElement, rightElement).map((_, count))
        (newTemplateElements, (ruleChar, count))
    }.unzip match {
      case (newTemplateElements, newCharsCount) =>
        val updatedTemplateElements = aggregateCount(newTemplateElements.flatten)
        val updatedCharsCount = aggregateCount(charsCount.toSeq ++ newCharsCount)
        (updatedTemplateElements, updatedCharsCount)
    }

  private def aggregateCount[T](elements: Seq[(T, Long)]) =
    elements
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).sum).toMap

  private def minMaxCount(templateElements: Map[Char, Long]) =
    templateElements.foldLeft((Long.MaxValue, Long.MinValue)) {
      case ((minCount, maxCount), (_, count)) => (minCount.min(count), maxCount.max(count))
    }
}
