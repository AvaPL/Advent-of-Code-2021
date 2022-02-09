package day23

import com.softwaremill.quicklens._
import day23.Amphipods.{Hallway, Room}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

sealed trait Amphipod {
  def energyPerMove: Int
  def destinationRoom: Int
}

case object A extends Amphipod {
  override def energyPerMove: Int = 1
  override def destinationRoom: Int = 0
}
case object B extends Amphipod {
  override def energyPerMove: Int = 10
  override def destinationRoom: Int = 1
}
case object C extends Amphipod {
  override def energyPerMove: Int = 100
  override def destinationRoom: Int = 2
}
case object D extends Amphipod {
  override def energyPerMove: Int = 1000
  override def destinationRoom: Int = 3
}

case class Amphipods(
    hallway: Hallway,
    rooms: Vector[Room]
)

object Amphipods {
  type Hallway = Vector[Option[Amphipod]]
  type Room = Vector[Option[Amphipod]]

  def apply(rooms: Vector[Vector[Amphipod]]): Amphipods =
    Amphipods(
      hallway = Vector.fill(11)(None),
      rooms = rooms.map(_.map(Some(_)))
    )
}

object Puzzle1 extends App {
  type Move = (Amphipods, Int)

//  val initialState = Amphipods(
//    rooms = Vector(
//      Vector(A, D),
//      Vector(C, D),
//      Vector(B, B),
//      Vector(A, C)
//    )
//  )
  // Example:
  val initialState = Amphipods(
    rooms = Vector(
      Vector(B, A),
      Vector(C, D),
      Vector(B, C),
      Vector(D, A)
    )
  )
  val expectedState = Amphipods(
    rooms = Vector(
      Vector(A, A),
      Vector(B, B),
      Vector(C, C),
      Vector(D, D)
    )
  )
  val minTotalEnergyCost = move(initialState, expectedState)
  println(minTotalEnergyCost)

  private def move(from: Amphipods, to: Amphipods) = {
    val upperBounds =
      mutable.Map[Amphipods, Double]().withDefaultValue(Double.PositiveInfinity)
    var minTotalEnergyCost = Int.MaxValue

    def loop(
        from: Amphipods,
        totalEnergyCost: Int = 0,
        visitedStates: Set[Amphipods] = Set.empty
    ): Int = {
      if (totalEnergyCost >= minTotalEnergyCost) Int.MaxValue
      else if (from == to) {
        println(s"Found! Total energy cost: $totalEnergyCost")
        minTotalEnergyCost = minTotalEnergyCost.min(totalEnergyCost)
        totalEnergyCost
      } else {
        val moves = ArrayBuffer.from(
          possibleMoves(from).collect {
            case move @ (state, _) if !visitedStates.contains(state) => move
          }
        )
        var minCost = Int.MaxValue
        while (moves.nonEmpty) {
          moves.sortInPlaceBy {
            case (state, energyCost) =>
              totalEnergyCost + energyCost + upperBounds(state)
          }
          val (state, energyCost) = moves.head
          val cost =
            loop(state, totalEnergyCost + energyCost, visitedStates + state)
          minCost = minCost.min(cost)
          moves.remove(0)
        }
        upperBounds.put(from, upperBounds(from).min(minCost))
        minCost
      }
    }

    loop(from)
    minTotalEnergyCost
  }

  private def possibleMoves(amphipods: Amphipods): Seq[Move] = {
    lazy val allInnerRoomMoves = innerRoomMoves(amphipods)
    lazy val allEntranceMoves = entranceMoves(amphipods)
    lazy val allHallwayMoves = hallwayMoves(amphipods)

    if (allInnerRoomMoves.nonEmpty) allInnerRoomMoves
    else if (allEntranceMoves.nonEmpty) allEntranceMoves
    else allHallwayMoves
  }

  private def innerRoomMoves(amphipods: Amphipods): Seq[Move] =
    amphipods.rooms.indices.flatMap {
      index =>
        val room = amphipods.rooms(index)
        room match {
          case Vector(Some(amphipod), None)
              if amphipod.destinationRoom == index => // Put correct one down
            Some(
              (
                amphipods
                  .modify(_.rooms.at(index))
                  .setTo(Vector(None, Some(amphipod))),
                amphipod.energyPerMove
              )
            )
          case Vector(None, Some(amphipod))
              if amphipod.destinationRoom != index => // Put incorrect one up
            Some(
              (
                amphipods
                  .modify(_.rooms.at(index))
                  .setTo(Vector(Some(amphipod), None)),
                amphipod.energyPerMove
              )
            )
          case _ => None
        }
    }

  private def entranceMoves(amphipods: Amphipods): Seq[Move] = {
    val entranceIndices = Seq(2, 4, 6, 8)
    entranceIndices.flatMap(entranceMove(amphipods, _))
  }

  private def entranceMove(amphipods: Amphipods, index: Int): Seq[Move] =
    amphipods.hallway(index) match {
      case Some(amphipod) =>
        val roomIndex = index / 2 - 1
        if (canMoveIntoRoom(amphipods, amphipod, roomIndex))
          Seq(
            (
              moveIntoRoom(amphipods, amphipod, index, roomIndex),
              amphipod.energyPerMove
            )
          )
        else
          moveLeftAndRight(amphipods, amphipod, index)
      case None => Seq.empty
    }

  private def canMoveIntoRoom(
      amphipods: Amphipods,
      amphipod: Amphipod,
      roomIndex: Int
  ) =
    amphipod.destinationRoom == roomIndex &&
      amphipods.rooms(roomIndex)(0).isEmpty &&
      amphipods.rooms(roomIndex)(1).forall(_.destinationRoom == roomIndex)

  private def moveIntoRoom(
      amphipods: Amphipods,
      amphipod: Amphipod,
      index: Int,
      roomIndex: Int
  ) =
    amphipods
      .modify(_.hallway.at(index))
      .setTo(None)
      .modify(_.rooms.at(roomIndex).at(0))
      .setTo(Some(amphipod))

  private def moveLeftAndRight(
      amphipods: Amphipods,
      amphipod: Amphipod,
      index: Int
  ): Seq[Move] = {
    val withAmphipodMovedLeft =
      moveAmphipod(amphipods, amphipod, index, index - 1)
    val withAmphipodMovedRight =
      moveAmphipod(amphipods, amphipod, index, index + 1)
    (withAmphipodMovedLeft ++ withAmphipodMovedRight).toSeq
  }

  private def moveAmphipod(
      amphipods: Amphipods,
      amphipod: Amphipod,
      fromIndex: Int,
      toIndex: Int
  ): Option[Move] =
    Option.when {
      0 <= toIndex && toIndex < amphipods.hallway.length &&
      amphipods.hallway(toIndex).isEmpty
    } {
      (
        amphipods
          .modify(_.hallway.at(fromIndex))
          .setTo(None)
          .modify(_.hallway.at(toIndex))
          .setTo(Some(amphipod)),
        amphipod.energyPerMove
      )
    }

  private def hallwayMoves(amphipods: Amphipods): Seq[Move] = {
    val allLeftAndRightMoves = leftAndRightMoves(amphipods)
    val allLeaveRoomMoves = leaveRoomMoves(amphipods)
    allLeftAndRightMoves ++ allLeaveRoomMoves
  }

  private def leftAndRightMoves(amphipods: Amphipods) =
    amphipods.hallway.indices.flatMap {
      index =>
        val amphipod = amphipods.hallway(index)
        amphipod
          .map(moveLeftAndRight(amphipods, _, index))
          .getOrElse(Seq.empty)
    }

  private def leaveRoomMoves(amphipods: Amphipods): Seq[Move] = {
    val entranceIndices = Seq(2, 4, 6, 8)
    entranceIndices.flatMap {
      index =>
        val roomIndex = index / 2 - 1
        amphipods.rooms(roomIndex)(0) match {
          case Some(amphipod)
              if shouldLeaveRoom(amphipods, amphipod, roomIndex) =>
            Some(
              (
                amphipods
                  .modify(_.rooms.at(roomIndex).at(0))
                  .setTo(None)
                  .modify(_.hallway.at(index))
                  .setTo(Some(amphipod)),
                amphipod.energyPerMove
              )
            )
          case _ => None
        }
    }
  }

  private def shouldLeaveRoom(
      amphipods: Amphipods,
      amphipod: Amphipod,
      roomIndex: Int
  ) =
    amphipod.destinationRoom != roomIndex ||
      amphipods.rooms(roomIndex)(1).exists(_.destinationRoom != roomIndex)
}
