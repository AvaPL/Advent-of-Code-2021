package day19

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics._
import util.InputParser._
import util.{FileReader, InputParser}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.Pi

case class Scanner(beacons: Seq[DenseVector[Int]]) {
  def translationAndRotationTo(
      other: Scanner
  ): Option[(DenseVector[Int], DenseMatrix[Int])] = {
    var translationRotation = Option.empty[(DenseVector[Int], DenseMatrix[Int])]
    for {
      rotation <- Scanner.rotations.toSeq
      rotatedBeacons = beacons.map(rotation * _)
      rotatedBeacon <- rotatedBeacons
      otherBeacon <- other.beacons
      translationToOtherHead = otherBeacon - rotatedBeacon
      if translationRotation.isEmpty
    } {
      val translatedBeacons = rotatedBeacons.map(_ + translationToOtherHead)
      val intersection = translatedBeacons.toSet.intersect(other.beacons.toSet)
      if (intersection.size >= 12)
        translationRotation = Some((translationToOtherHead, rotation))
    }
    translationRotation
  }
}

object Scanner {
  val noRotation: DenseMatrix[Int] = DenseMatrix.eye[Int](3)

  val noTranslation: DenseVector[Int] = DenseVector.zeros[Int](3)

  lazy val rotations: Set[DenseMatrix[Int]] = {
    val degrees = List(0, Pi / 2, Pi, 3 * Pi / 2)
    val rotationMatrices = for {
      x <- degrees
      y <- degrees
      z <- degrees
    } yield rotation(x, y, z)
    rotationMatrices.toSet
  }

  private def rotation(x: Double, y: Double, z: Double) = {
    val xRotation = DenseMatrix(
      (1, 0, 0),
      (0, cos(x).toInt, -sin(x).toInt),
      (0, sin(x).toInt, cos(x).toInt)
    )
    val yRotation = DenseMatrix(
      (cos(y).toInt, 0, sin(y).toInt),
      (0, 1, 0),
      (-sin(y).toInt, 0, cos(y).toInt)
    )
    val zRotation = DenseMatrix(
      (cos(z).toInt, -sin(z).toInt, 0),
      (sin(z).toInt, cos(z).toInt, 0),
      (0, 0, 1)
    )
    xRotation * yRotation * zRotation
  }
}

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
