package day5

case class Point(x: Int, y: Int) {
  def add(other: Point): Point =
    Point(x + other.x, y + other.y)
}
