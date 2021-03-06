package day19

import breeze.linalg.{DenseMatrix, DenseVector}
import util.InputParser._
import util.{FileReader, InputParser}

import scala.annotation.tailrec
import scala.collection.mutable

object Puzzle1InputParser extends InputParser[Seq[Scanner]] {
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

object Puzzle1 extends App {
  val input = FileReader.readUnsafe("input/day19/puzzle1.txt")
  val scanners = Puzzle1InputParser.parse(input)
  val withTranslationsAndRotations = translationsAndRotations(
    knownScanners =
      Seq((Scanner.noTranslation, Scanner.noRotation, scanners.head)),
    unknownScanners = scanners.drop(1)
  )
  val translatedAndRotatedBeacons = withTranslationsAndRotations.flatMap {
    case (translation, rotation, Scanner(beacons)) =>
      beacons.map(rotation * _ + translation)
  }.toSet
  println(translatedAndRotatedBeacons.size)

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
