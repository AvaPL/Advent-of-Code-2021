package day12

object Cave {
  val startName = "start"
  val endName = "end"

  def isSmall(name: String): Boolean =
    name.head.isLower
}
