package day5

import scala.collection.mutable

case class VentsMap(size: Int) {

  private val coordinates: mutable.Seq[mutable.Seq[Int]] =
    mutable.Seq.fill(size)(mutable.Seq.fill(size)(0))

  def markPoint(point: Point): Unit =
    coordinates(point.x)(point.y) += 1

  def countDangerousAreas: Int =
    coordinates.flatten.count(_ > 1)
}
