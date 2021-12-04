package day4

import com.softwaremill.quicklens._

case class BingoBoard(fields: Seq[Seq[BingoField]]) {
  lazy val isWinning: Boolean = {
    fields.exists(_.forall(_.isMarked)) ||
      fields.transpose.exists(_.forall(_.isMarked))
  }

  def mark(value: Int): BingoBoard =
    this
      .modify(_.fields.each.each)
      .using(field => if (field.value == value) field.mark else field)
}
