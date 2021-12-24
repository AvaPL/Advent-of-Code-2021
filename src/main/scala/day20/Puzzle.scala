package day20

import util.InputParser._
import util.{FileReader, InputParser}

sealed trait Pixel

case object Dark extends Pixel

case object Light extends Pixel

object PuzzleInputParser extends InputParser[(Seq[Pixel], Seq[Seq[Pixel]])] {

  override def parse(string: String): (Seq[Pixel], Seq[Seq[Pixel]]) = {
    string.splitBlocks match {
      case Seq(enhancementAlgorithmRaw, inputImageRaw) =>
        val enhancementAlgorithm = enhancementAlgorithmRaw.map(charToPixel)
        val inputImage = for {
          pixelRow <- inputImageRaw.splitLines
        } yield for {
          pixelChar <- pixelRow
        } yield charToPixel(pixelChar)
        (enhancementAlgorithm, inputImage)
    }
  }

  private def charToPixel(char: Char) =
    char match {
      case '.' => Dark
      case '#' => Light
    }
}

object Puzzle extends App {
  val input = FileReader.readUnsafe("input/day20/puzzle.txt")
  val (enhancementAlgorithm, inputImage) = PuzzleInputParser.parse(input)
  val puzzle1 = solvePuzzle(repeat = 2)
  println(s"puzzle1 = $puzzle1")
  val puzzle2 = solvePuzzle(repeat = 50)
  println(s"puzzle2 = $puzzle2")

  private def solvePuzzle(repeat: Int) = {
    val enhancedImage = enhanceImage(inputImage, repeat)
    enhancedImage.flatten.count(_ == Light)
  }

  private def enhanceImage(image: Seq[Seq[Pixel]], repeat: Int): Seq[Seq[Pixel]] = {
    var enhancedImage = image
    var filler: Pixel = Dark
    for (_ <- 1 to repeat) {
      enhancedImage = enhanceImage(enhancedImage, filler)
      filler = filler match {
        case _ if enhancementAlgorithm.head == Dark => Dark
        case Dark => Light
        case Light => Dark
      }
    }
    enhancedImage
  }

  private def enhanceImage(image: Seq[Seq[Pixel]], filler: Pixel): Seq[Seq[Pixel]] = {
    val fillerCount = 2
    val withCanvasExpanded = expandCanvas(image, filler, fillerCount)
    val enhancedImage = withCanvasExpanded.zipWithIndex.map {
      case (pixelRow, x) => pixelRow.zipWithIndex.map {
        case (pixel, y) =>
          if (isEdge(withCanvasExpanded, x, y)) pixel
          else enhancePixel(withCanvasExpanded, x, y)
      }
    }
    enhancedImage.dropSurrounding(fillerCount - 1).map(_.dropSurrounding(fillerCount - 1))
  }

  private def expandCanvas(image: Seq[Seq[Pixel]], filler: Pixel, fillerCount: Int) = {
    val fillerPixelsRow = Seq.fill(image.head.length + fillerCount * 2)(filler)
    val withFillerPixelColumns = image.map(_.surroundWith(filler, fillerCount))
    withFillerPixelColumns.surroundWith(fillerPixelsRow, fillerCount)
  }

  private def isEdge(image: Seq[Seq[Pixel]], x: Int, y: Int) =
    x == 0 || y == 0 || x == image.length - 1 || y == image.head.length - 1

  private def enhancePixel(image: Seq[Seq[Pixel]], x: Int, y: Int) = {
    val index = enhancementIndex(image, x, y)
    enhancementAlgorithm(index)
  }

  private def enhancementIndex(image: Seq[Seq[Pixel]], x: Int, y: Int) = {
    val indexString = (for {
      i <- x - 1 to x + 1
      j <- y - 1 to y + 1
    } yield pixelToBinary(image(i)(j))).mkString
    val index = Integer.parseInt(indexString, 2)
    index
  }

  private def pixelToBinary(pixel: Pixel) =
    pixel match {
      case Dark => 0
      case Light => 1
    }

  implicit class SeqOps[T](val seq: Seq[T]) {
    def surroundWith(element: T, count: Int = 1): Seq[T] = {
      Seq.fill(count)(element) ++ seq ++ Seq.fill(count)(element)
    }

    def dropSurrounding(count: Int = 1): Seq[T] =
      seq.drop(count).dropRight(count)
  }
}
