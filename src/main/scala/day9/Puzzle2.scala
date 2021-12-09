package day9

import util.{FileReader, InputParser}
import util.InputParser._

import scala.collection.mutable

object Puzzle2InputParser extends InputParser[mutable.Seq[mutable.Seq[HeightmapField]]] {
  override def parse(string: String): mutable.Seq[mutable.Seq[HeightmapField]] =
    (for {
      line <- string.splitLines
    } yield (for {
      value <- line.splitBy("")
    } yield HeightmapField(value.toInt)
      ).to(mutable.Seq)
      ).to(mutable.Seq)
}

object Puzzle2 extends App {
  val input = FileReader.readUnsafe("input/day9/puzzle2.txt")
  val heightmap = Puzzle2InputParser.parse(input)
  val basinSizes = mutable.ArrayBuffer[Int]()
  markBasins()
  basinSizes.sortInPlace
  val largestBasinsProduct = basinSizes.takeRight(3).product
  println(largestBasinsProduct)

  private def markBasins(): Unit =
    for (i <- heightmap.indices) {
      for (j <- heightmap(i).indices) {
        val field = heightmap(i)(j)
        if (!field.isMarked) {
          if (field.value != 9)
            basinSizes.append(0)
          markBasin(i, j)
        }
      }
    }

  private def markBasin(i: Int, j: Int): Unit = {
    val field = heightmap(i)(j)
    if (!field.isMarked) {
      heightmap(i)(j) = field.mark
      if (field.value != 9) {
        basinSizes(basinSizes.length - 1) += 1
        val up = (heightmap(i).lift(j + 1), i, j + 1)
        val down = (heightmap(i).lift(j - 1), i, j - 1)
        val left = (heightmap.lift(i - 1).map(_ (j)), i - 1, j)
        val right = (heightmap.lift(i + 1).map(_ (j)), i + 1, j)
        List(up, down, left, right).collect {
          case (Some(HeightmapField(_, false)), i, j) => markBasin(i, j)
        }
      }
    }
  }
}
