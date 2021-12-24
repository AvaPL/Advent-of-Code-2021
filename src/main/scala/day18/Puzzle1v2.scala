package day18

import util.InputParser._
import util.{FileReader, InputParser}

object Puzzle1InputParser extends InputParser[Seq[SnailfishNumber]] {
  override def parse(string: String): Seq[SnailfishNumber] =
    string.splitLines.map { line =>
      val list = new SnailfishNumber()
      var depth = 0
      for (char <- line) {
        char match {
          case '[' => depth += 1
          case ']' => depth -= 1
          case ',' =>
          case digitChar =>
            val value = digitChar.asDigit
            list.offerLast(Node(value, depth))
        }
      }
      list
    }
}

// This solution uses stateful operations, it's more Java-ish that idiomatic Scala
object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day18/puzzle1.txt")
  val numbers = Puzzle1InputParser.parse(input)
  val numbersSum = sum(numbers)
  val sumMagnitude = magnitude(numbersSum)
  println(sumMagnitude)

  private def sum(numbers: Seq[SnailfishNumber]): SnailfishNumber = {
    val result = new SnailfishNumber()
    result.addAll(numbers.head)
    numbers.tail.foreach { number =>
      result.addAll(number)
      incrementDepth(result)
      reduceNumber(result)
    }
    result
  }

  private def incrementDepth(number: SnailfishNumber): Unit = {
    val iterator = number.listIterator()
    while (iterator.hasNext) {
      val node = iterator.next()
      iterator.set(node.copy(depth = node.depth + 1))
    }
  }

  private def reduceNumber(number: SnailfishNumber): Unit = {
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

  private def explode(iterator: SnailfishNumberIterator): Unit = {
    while (iterator.hasNext) {
      val left = iterator.next()
      if (left.depth > 4)
        explodeCurrentPair(iterator, left)
    }
  }

  private def explodeCurrentPair(iterator: SnailfishNumberIterator, left: Node): Unit = {
    iterator.remove()
    addToLeft(iterator, left.value)
    val right = iterator.next()
    iterator.set(Node(0, 4))
    addToRight(iterator, right.value)
    iterator.previous()
    iterator.previous()
  }

  private def addToLeft(iterator: SnailfishNumberIterator, value: Int): Unit =
    if (iterator.hasPrevious) {
      val previous = iterator.previous()
      iterator.set(previous.copy(value = previous.value + value))
      iterator.next()
    }

  private def addToRight(iterator: SnailfishNumberIterator, value: Int): Unit =
    if (iterator.hasNext) {
      val next = iterator.next()
      iterator.set(next.copy(value = next.value + value))
    }

  private def split(iterator: SnailfishNumberIterator): Unit = {
    var splitOccurred = false
    while (!splitOccurred && iterator.hasNext) {
      val number = iterator.next()
      if (number.value >= 10) {
        splitNumber(iterator, number)
        splitOccurred = true
      }
    }
  }

  private def splitNumber(iterator: SnailfishNumberIterator, number: Node): Unit = {
    val left = number.value / 2
    val right = number.value / 2 + number.value % 2
    val depth = number.depth + 1
    iterator.set(Node(left, depth))
    iterator.add(Node(right, depth))
    iterator.previous()
    iterator.previous()
  }

  private def magnitude(number: SnailfishNumber) = {
    val iterator = number.listIterator()
    var previous = iterator.next()
    while (iterator.hasNext) {
      val next = iterator.next()
      if (next.depth == previous.depth) {
        val magnitude = calculateMagnitude(iterator, previous, next)
        previous = setPrevious(iterator, previous, magnitude)
      } else previous = next
    }
    number.get(0).value
  }

  private def calculateMagnitude(iterator: MagnitudeIterator, previous: Node, next: Node) = {
    iterator.remove()
    iterator.previous()
    val magnitude = Node(3 * previous.value + 2 * next.value, previous.depth - 1)
    iterator.set(magnitude)
    magnitude
  }

  private def setPrevious(iterator: MagnitudeIterator, previous: Node, magnitude: Node): Node = {
    var newPrevious = previous
    if (iterator.hasPrevious) {
      newPrevious = iterator.previous()
      iterator.next()
    } else if (magnitude.depth > 0)
      newPrevious = iterator.next()
    newPrevious
  }
}
