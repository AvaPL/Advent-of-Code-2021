package day4

import util.InputParser._
import util.{FileReader, InputParser}
import com.softwaremill.quicklens._

import scala.annotation.tailrec

object Puzzle2InputParser extends InputParser[(Seq[Int], Seq[BingoBoard])] {
  override def parse(string: String): (Seq[Int], Seq[BingoBoard]) =
    string.splitBlocks match {
      case Seq(drawnNumbersRaw, boardsRaw@_*) =>
        val drawnNumbers = parseDrawnNumbers(drawnNumbersRaw)
        val boards = boardsRaw.map(parseBingoBoard)
        (drawnNumbers, boards)
    }

  private def parseDrawnNumbers(drawnNumbersRaw: String) =
    drawnNumbersRaw.splitBy(",").map(_.toInt)

  private def parseBingoBoard(boardRaw: String) = {
    val fields = for {
      rows <- boardRaw.splitLines
    } yield for {
      fieldValue <- rows.trim.splitByRegex("\\s+")
    } yield BingoField(fieldValue.toInt)
    BingoBoard(fields)
  }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day4/puzzle2.txt")
  val (drawnNumbers, boards) = Puzzle2InputParser.parse(input)
  val (lastNumber, lastWinningBoard) = chooseLastWinningBoard(drawnNumbers, boards)
  val unmarkedFieldsSum = lastWinningBoard.fields.flatten.filterNot(_.isMarked).map(_.value).sum
  println(unmarkedFieldsSum * lastNumber)

  @tailrec
  def chooseLastWinningBoard(drawnNumbers: Seq[Int], boards: Seq[BingoBoard]): (Int, BingoBoard) = {
    val number = drawnNumbers.head
    val updatedBoards = boards.map(_.mark(number))
    val notWinningBoards = updatedBoards.filterNot(_.isWinning)
    if (notWinningBoards.isEmpty)
      (number, updatedBoards.head)
    else
      chooseLastWinningBoard(drawnNumbers.tail, notWinningBoards)
  }
}
