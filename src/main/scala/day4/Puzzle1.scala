package day4

import util.InputParser._
import util.{FileReader, InputParser}

import scala.annotation.tailrec

object Puzzle1InputParser extends InputParser[(Seq[Int], Seq[BingoBoard])] {
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

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day4/puzzle1.txt")
  val (drawnNumbers, boards) = Puzzle1InputParser.parse(input)
  val (lastNumber, firstWinningBoard) = chooseFirstWinningBoard(drawnNumbers, boards)
  val unmarkedFieldsSum = firstWinningBoard.fields.flatten.filterNot(_.isMarked).map(_.value).sum
  println(unmarkedFieldsSum * lastNumber)

  @tailrec
  def chooseFirstWinningBoard(drawnNumbers: Seq[Int], boards: Seq[BingoBoard]): (Int, BingoBoard) = {
    val number = drawnNumbers.head
    val updatedBoards = boards.map(_.mark(number))
    val winningBoard = updatedBoards.find(_.isWinning)
    winningBoard match {
      case Some(board) => (number, board)
      case None => chooseFirstWinningBoard(drawnNumbers.tail, updatedBoards)
    }
  }
}
