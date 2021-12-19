package day18

import util.InputParser._
import util.{FileReader, InputParser}

import java.util

case class SnailfishNumberNode(value: Int, depth: Int)

object Puzzle1v2InputParser extends InputParser[Seq[util.LinkedList[SnailfishNumberNode]]] {
  override def parse(string: String): Seq[util.LinkedList[SnailfishNumberNode]] =
    string.splitLines.map { line =>
      val list = new util.LinkedList[SnailfishNumberNode]()
      var depth = 0
      for (char <- line) {
        char match {
          case '[' => depth += 1
          case ']' => depth -= 1
          case ',' =>
          case digitChar =>
            val value = digitChar.asDigit
            list.offerLast(SnailfishNumberNode(value, depth))
        }
      }
      list
    }
}

object Puzzle1v2 extends App {
  val input = FileReader.readUnsafe("input/day18/puzzlex.txt")
  val numbers = Puzzle1v2InputParser.parse(input)
  numbers.foreach(reduceNumber)
  println(numbers)

  private def reduceNumber(snailfishNumber: util.LinkedList[SnailfishNumberNode]): Boolean = {
    val iterator = snailfishNumber.listIterator()
    var reduced = false
    while (!reduced && iterator.hasNext) {
      val left = iterator.next()
      if (left.depth > 4) {
        reduceCurrentPair(iterator, left)
        reduced = true
      }
    }
    reduced
  }

  private def reduceCurrentPair(iterator: util.ListIterator[SnailfishNumberNode], left: SnailfishNumberNode): Unit = {
    iterator.remove()
    addToLeft(iterator, left.value)
    val right = iterator.next()
    iterator.set(SnailfishNumberNode(0, 4))
    addToRight(iterator, right.value)
  }

  private def addToLeft(iterator: util.ListIterator[SnailfishNumberNode], value: Int): Unit =
    if (iterator.hasPrevious) {
      val previous = iterator.previous()
      iterator.set(previous.copy(value = previous.value + value))
      iterator.next()
    }

  private def addToRight(iterator: util.ListIterator[SnailfishNumberNode], value: Int): Unit =
    if (iterator.hasNext) {
      val next = iterator.next()
      iterator.set(next.copy(value = next.value + value))
    }
}
