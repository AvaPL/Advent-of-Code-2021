package day18

import util.InputParser._
import util.{FileReader, InputParser}

import java.util

case class SnailfishNumberNode(value: Int, depth: Int)

// TODO: Simplify util.* types

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
  val numbersSum = sum(numbers)
  println(numbersSum)

  private def sum(numbers: Seq[util.LinkedList[SnailfishNumberNode]]): util.LinkedList[SnailfishNumberNode] = {
    val result = new util.LinkedList[SnailfishNumberNode]()
    result.addAll(numbers.head)
    numbers.tail.foreach { number =>
      result.addAll(number)
      incrementDepth(result)
      reduceNumber(result)
    }
    result
  }

  private def incrementDepth(number: util.LinkedList[SnailfishNumberNode]): Unit = {
    val iterator = number.listIterator()
    while (iterator.hasNext) {
      val node = iterator.next()
      iterator.set(node.copy(depth = node.depth + 1))
    }
  }

  private def reduceNumber(number: util.LinkedList[SnailfishNumberNode]): Unit = {
    var needsReduction = true
    var iterator = number.listIterator()
    while (needsReduction) {
      explode(iterator)
      iterator = number.listIterator()
      split(iterator)
      if (!iterator.hasNext)
        needsReduction = false
    }
  }

  private def explode(iterator: util.ListIterator[SnailfishNumberNode]): Unit = {
    while (iterator.hasNext) {
      val left = iterator.next()
      if (left.depth > 4)
        explodeCurrentPair(iterator, left)
    }
  }

  /**
   * Assumes that depth is 5. After the explosion iterator is placed before the exploded number.
   *
   * @example
   * Before: [[[[3, [1, 2]], 4], 5], 6]
   * ----------------^
   * After: [[[4, 0], 6], 5], 6]
   * ----------^
   *
   */
  private def explodeCurrentPair(iterator: util.ListIterator[SnailfishNumberNode], left: SnailfishNumberNode): Unit = {
    iterator.remove()
    addToLeft(iterator, left.value)
    val right = iterator.next()
    iterator.set(SnailfishNumberNode(0, 4))
    addToRight(iterator, right.value)
    iterator.previous()
    iterator.previous()
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

  private def split(iterator: util.ListIterator[SnailfishNumberNode]): Unit = {
    var splitOccurred = false
    while (!splitOccurred && iterator.hasNext) {
      val number = iterator.next()
      if (number.value >= 10) {
        splitNumber(iterator, number)
        splitOccurred = true
      }
    }
  }

  /**
   * Splits only one number. After the split iterator is placed on before the split result.
   *
   * @example
   * Before: [3, 11]
   * ------------^
   * After: [3, [5, 6]]
   * --------^
   */
  private def splitNumber(iterator: util.ListIterator[SnailfishNumberNode], number: SnailfishNumberNode): Unit = {
    val left = number.value / 2
    val right = number.value / 2 + number.value % 2
    val depth = number.depth + 1
    iterator.set(SnailfishNumberNode(left, depth))
    iterator.add(SnailfishNumberNode(right, depth))
    iterator.previous()
    iterator.previous()
  }
}
