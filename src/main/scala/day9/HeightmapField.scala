package day9

case class HeightmapField(value: Int, isMarked: Boolean = false) {
  lazy val mark: HeightmapField = copy(isMarked = true)
}
