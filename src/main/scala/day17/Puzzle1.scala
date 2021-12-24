package day17

object Puzzle1 extends App {
  val topLeft = (138, -71)
  val bottomRight = (184, -125)
  val maxHeight = (-bottomRight._2 - 1) * (-bottomRight._2) / 2
  println(maxHeight)
}
