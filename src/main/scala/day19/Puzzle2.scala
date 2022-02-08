package day19

import breeze.linalg.{DenseMatrix, DenseVector}
import util.InputParser._
import util.{FileReader, InputParser}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.abs

object Puzzle2InputParser extends InputParser[Seq[Scanner]] {
  override def parse(string: String): Seq[Scanner] =
    string.splitBlocks.map {
      block =>
        val beacons =
          block.splitLines
            .drop(1)
            .map(_.splitBy(","))
            .map {
              case Seq(x, y, z) => DenseVector(x.toInt, y.toInt, z.toInt)
            }
        Scanner(beacons)
    }
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day19/puzzle2.txt")
  val scanners = Puzzle2InputParser.parse(input)
  val withTranslationsAndRotations = translationsAndRotations(
    knownScanners =
      Seq((Scanner.noTranslation, Scanner.noRotation, scanners.head)),
    unknownScanners = scanners.drop(1)
  )
  val scannersPositions = withTranslationsAndRotations.map {
    case (translation, _, _) => translation
  }
  val largestManhattanDistance = scannersPositions
    .combinations(2)
    .map {
      case Seq(first, second) => (second - first).map(abs).data.sum
    }
    .max
  println(largestManhattanDistance)

  @tailrec
  private def translationsAndRotations(
      knownScanners: Seq[(DenseVector[Int], DenseMatrix[Int], Scanner)],
      unknownScanners: Seq[Scanner]
  ): Seq[(DenseVector[Int], DenseMatrix[Int], Scanner)] = {
    val newKnown = mutable.ListBuffer(knownScanners: _*)
    val newUnknown = mutable.ListBuffer[Scanner]()
    for (unknownScanner <- unknownScanners) {
      val positionFound = knownScanners.exists {
        known =>
          unknownScanner.translationAndRotationTo(known._3) match {
            case Some((translation, rotation)) =>
              newKnown.addOne(
                (
                  known._1 + known._2 * translation,
                  known._2 * rotation,
                  unknownScanner
                )
              )
              true
            case None => false
          }
      }
      if (!positionFound)
        newUnknown.addOne(unknownScanner)
    }
    if (newUnknown.isEmpty) newKnown.toSeq
    else translationsAndRotations(newKnown.toSeq, newUnknown.toSeq)
  }
}
