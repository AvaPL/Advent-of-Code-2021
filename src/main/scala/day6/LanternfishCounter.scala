package day6

import scala.collection.mutable

object LanternfishCounter {

  def count(lanternfish: Seq[Int], days: Int): Long = {
    val lanternfishToCountMap = countMap(lanternfish)
    for (_ <- 1 to days) {
      val newLanternfish = lanternfishToCountMap.getOrElse(0, 0L)
      shiftDays(lanternfishToCountMap)
      addNewLanternfish(lanternfishToCountMap, newLanternfish)
    }
    lanternfishToCountMap.values.sum
  }

  private def countMap(lanternfish: Seq[Int]) =
    lanternfish.groupBy(identity).view.mapValues(_.length.toLong).to(mutable.Map)

  private def shiftDays(lanternfishToCountMap: mutable.Map[Int, Long]): Unit =
    for (days <- 1 to 8) {
      val newCount = lanternfishToCountMap.getOrElse(days, 0L)
      lanternfishToCountMap.update(days - 1, newCount)
    }

  private def addNewLanternfish(lanternfishToCountMap: mutable.Map[Int, Long], newLanternfish: Long): Unit = {
    val newDay6 = lanternfishToCountMap.getOrElse(6, 0L) + newLanternfish
    lanternfishToCountMap.update(6, newDay6)
    lanternfishToCountMap.update(8, newLanternfish)
  }
}
