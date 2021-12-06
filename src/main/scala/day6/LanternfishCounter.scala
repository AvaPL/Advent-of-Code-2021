package day6

object LanternfishCounter {

  def count(lanternfish: Seq[Int], days: Int): Long = {
    val countArray = lanternfishCountArray(lanternfish)
    for (_ <- 1 to days) {
      val newLanternfish = countArray(0)
      shiftDays(countArray)
      addNewLanternfish(countArray, newLanternfish)
    }
    countArray.sum
  }

  private def lanternfishCountArray(lanternfish: Seq[Int]) = {
    val array = Array.fill(9)(0L)
    lanternfish.groupBy(identity).foreach {
      case (i, value) => array(i) = value.length
    }
    array
  }

  private def shiftDays(countArray: Array[Long]): Unit =
    for (days <- 1 to 8) {
      val newCount = countArray(days)
      countArray(days - 1) = newCount
    }

  private def addNewLanternfish(countArray: Array[Long], newLanternfish: Long): Unit = {
    countArray(6) += newLanternfish
    countArray(8) = newLanternfish
  }
}
