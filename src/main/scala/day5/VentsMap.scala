package day5

import com.softwaremill.quicklens._

case class VentsMap private(coordinates: Seq[Seq[Int]]) {

  def markPoint(point: Point): VentsMap =
    this
      .modify(_.coordinates.at(point.x).at(point.y))
      .using(_ + 1)

  def markLine(line: Line): VentsMap =
    line.coveredPoints.foldLeft(this)(_.markPoint(_))

  def countDangerousAreas: Int =
    coordinates.flatten.count(_ > 1)
}

object VentsMap {
  def apply(size: Int): VentsMap =
    VentsMap(Seq.fill(size)(Seq.fill(size)(0)))
}
