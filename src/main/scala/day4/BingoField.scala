package day4

case class BingoField(value: Int, isMarked: Boolean = false) {
  lazy val mark: BingoField = copy(isMarked = true)
}
