package day23

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
}

object Puzzle1 extends App {
  type State = Map[Amphipod, Position]

//  val initialState: State = Map(
//  Position(Room.A.index, 1) -> Amphipod.A,
//  Position(Room.A.index, 2) -> Amphipod.D,
//  Position(Room.B.index, 1) -> Amphipod.C,
//  Position(Room.B.index, 2) -> Amphipod.D,
//  Position(Room.C.index, 1) -> Amphipod.B,
//  Position(Room.C.index, 2) -> Amphipod.B,
//  Position(Room.D.index, 1) -> Amphipod.A,
//  Position(Room.D.index, 2) -> Amphipod.C
//  )
  // Example:
  val initialState: State = Map(
    Position(Room.A.index, 1) -> Amphipod.B,
    Position(Room.A.index, 2) -> Amphipod.A,
    Position(Room.B.index, 1) -> Amphipod.C,
    Position(Room.B.index, 2) -> Amphipod.D,
    Position(Room.C.index, 1) -> Amphipod.B,
    Position(Room.C.index, 2) -> Amphipod.C,
    Position(Room.D.index, 1) -> Amphipod.D,
    Position(Room.D.index, 2) -> Amphipod.A
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
}
