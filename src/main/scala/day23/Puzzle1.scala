package day23

import scala.collection.mutable
import scala.math.abs

sealed trait Room {
  def index: Int
}

object Room {
  case object A extends Room {
    override def index: Int = 2
  }
  case object B extends Room {
    override def index: Int = 4
  }
  case object C extends Room {
    override def index: Int = 6
  }
  case object D extends Room {
    override def index: Int = 8
  }
}

sealed trait Amphipod {
  def energyPerMove: Int
  def destinationRoom: Room
}

object Amphipod {
  case object A extends Amphipod {
    override def energyPerMove: Int = 1
    override def destinationRoom: Room = Room.A
  }
  case object B extends Amphipod {
    override def energyPerMove: Int = 10
    override def destinationRoom: Room = Room.B
  }
  case object C extends Amphipod {
    override def energyPerMove: Int = 100
    override def destinationRoom: Room = Room.C
  }
  case object D extends Amphipod {
    override def energyPerMove: Int = 1000
    override def destinationRoom: Room = Room.D
  }
}

case class Position(x: Int, y: Int) {
  def isHallway: Boolean = y == 0
  def isRoom(amphipod: Amphipod): Boolean =
    x == amphipod.destinationRoom.index && !isHallway
}

object Position {
  val xRange: Range.Inclusive = 0 to 10
  val yRange: Range.Inclusive = 0 to 2
  val hallwayY = 0
  val roomYs: Range.Inclusive = 1 to 2
  val roomIndices: Seq[Int] = Seq(Room.A, Room.B, Room.C, Room.D).map(_.index)
  val roomPositions: Seq[Position] = roomIndices.flatMap {
    x => roomYs.map(Position(x, _))
  }
  val hallwayStops: Seq[Int] = xRange.diff(roomIndices)
  val hallwayStopsPositions: Seq[Position] =
    hallwayStops.map(Position(_, hallwayY))

  def roomPositions(room: Room): Seq[Position] =
    roomPositions.filter(_.x == room.index)
}

object Puzzle1 extends App {
  type State = Map[Position, Amphipod]

  val initialState: State = Map(
    Position(Room.A.index, 1) -> Amphipod.A,
    Position(Room.A.index, 2) -> Amphipod.D,
    Position(Room.B.index, 1) -> Amphipod.C,
    Position(Room.B.index, 2) -> Amphipod.D,
    Position(Room.C.index, 1) -> Amphipod.B,
    Position(Room.C.index, 2) -> Amphipod.B,
    Position(Room.D.index, 1) -> Amphipod.A,
    Position(Room.D.index, 2) -> Amphipod.C
  )
  val expectedState: State = Map(
    Position(Room.A.index, 1) -> Amphipod.A,
    Position(Room.A.index, 2) -> Amphipod.A,
    Position(Room.B.index, 1) -> Amphipod.B,
    Position(Room.B.index, 2) -> Amphipod.B,
    Position(Room.C.index, 1) -> Amphipod.C,
    Position(Room.C.index, 2) -> Amphipod.C,
    Position(Room.D.index, 1) -> Amphipod.D,
    Position(Room.D.index, 2) -> Amphipod.D
  )
  val energyCost = dijkstra(initialState, expectedState)
  println(energyCost)

  private def dijkstra(initialState: State, expectedState: State) = {
    val cheapestCosts = mutable.Map[State, Int]().withDefaultValue(Int.MaxValue)
    val visitedStates = mutable.PriorityQueue[(State, Int)]()(Ordering.by(_._2))
    visitedStates.enqueue((initialState, 0))
    while (visitedStates.nonEmpty) {
      val (state, energyCost) = visitedStates.dequeue
      val nextStates = possibleMoves(state)
      nextStates.foreach {
        nextState =>
          val nextEnergyCost = energyCost + moveCost(state, nextState)
          if (nextEnergyCost < cheapestCosts(nextState)) {
            cheapestCosts.update(nextState, nextEnergyCost)
            visitedStates.enqueue((nextState, nextEnergyCost))
          }
      }
    }
    cheapestCosts(expectedState)
  }

  private[day23] def possibleMoves(state: State) = {
    lazy val toRoomMoves = possibleToRoomMoves(state)
    lazy val toHallwayMoves = possibleToHallwayMoves(state)

    if (toRoomMoves.nonEmpty) toRoomMoves else toHallwayMoves
  }

  private def possibleToRoomMoves(state: State) = {
    val hallwayStopsAmphipods = Position.hallwayStopsPositions.flatMap {
      stop =>
        state.find(_._1 == stop)
    }
    val topAmphipods = topSpotsAmphipods(state)
    val positionChanges = (hallwayStopsAmphipods ++ topAmphipods).flatMap {
      case (position, amphipod) =>
        val destination = spotToMoveIn(state, amphipod)
        destination.map((position, _, amphipod))
    }
    positionChangesToStates(state, positionChanges)
  }

  private def topSpotsAmphipods(state: State) =
    Position.roomPositions.groupBy(_.x).values.flatMap {
      positions =>
        val occupiedPositions = positions.flatMap {
          position =>
            state.find(_._1 == position)
        }
        occupiedPositions.minByOption(_._1.y)
    }

  private def spotToMoveIn(state: State, amphipod: Amphipod) = {
    val roomSpots = Position.roomPositions(amphipod.destinationRoom)
    val occupiedSpots = roomSpots.flatMap {
      spot =>
        state.find(_._1 == spot)
    }.toMap
    val roomContainsOnlyValidAmphipods =
      occupiedSpots.values.forall(_ == amphipod)
    if (roomContainsOnlyValidAmphipods) // get "deepest" free spot
      roomSpots.toSet
        .diff(occupiedSpots.keySet)
        .toList
        .maxByOption(_.y)
    else
      None
  }

  private def positionChangesToStates(
      state: State,
      positionChanges: Iterable[(Position, Position, Amphipod)]
  ) =
    positionChanges.collect {
      case (from, to, amphipod) if isPathValid(state, from, to) =>
        state.removed(from).updated(to, amphipod)
    }

  private def isPathValid(
      state: State,
      from: Position,
      to: Position
  ) = {
    val occupiedHallwayStops = state.keySet.filter(_.isHallway)
    val minX = from.x.min(to.x)
    val maxX = from.x.max(to.x)
    val hallwayStopsBetween = Position.hallwayStopsPositions.filter {
      case Position(x, _) => minX < x && x < maxX
    }.toSet
    occupiedHallwayStops.intersect(hallwayStopsBetween).isEmpty
  }

  private def possibleToHallwayMoves(state: State) = {
    val topAmphipods = topSpotsAmphipods(state)
    val amphipodsToMove = topAmphipods.filter {
      // amphipod in valid room, but may block other amphipods
      case (position, amphipod)
          if amphipod.destinationRoom.index == position.x =>
        !roomContainsOnlyValidAmphipods(state, amphipod.destinationRoom)
      // amphipod in invalid room
      case _ => true
    }
    val emptyHallwayPositions =
      Position.hallwayStopsPositions.filterNot(state.isDefinedAt)
    val positionChanges = amphipodsToMove.flatMap {
      case (position, amphipod) =>
        emptyHallwayPositions.map((position, _, amphipod))
    }
    positionChangesToStates(state, positionChanges)
  }

  private def roomContainsOnlyValidAmphipods(
      state: State,
      room: Room
  ) = {
    val roomSpots = Position.roomPositions(room)
    val occupiedSpotsAmphipods = roomSpots.flatMap(state.get)
    occupiedSpotsAmphipods.forall(_.destinationRoom == room)
  }

  private[day23] def moveCost(state1: State, state2: State) = {
    val state1Set = state1.toSet
    val state2Set = state2.toSet
    val List(position1 -> amphipod, position2 -> _) =
      state1Set.union(state2Set).diff(state1Set.intersect(state2Set)).toList
    val pathX = abs(position1.x - position2.x)
    val pathY = position1.y + position2.y
    val pathLength = pathX + pathY
    pathLength * amphipod.energyPerMove
  }
}
