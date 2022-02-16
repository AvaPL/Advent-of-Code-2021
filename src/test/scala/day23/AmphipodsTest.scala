package day23

import day23.Amphipods.State
import org.scalatest.Inspectors
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AmphipodsTest extends AnyWordSpec with Matchers with Inspectors {

  val amphipods: Amphipods = Amphipods(2)

  "possibleMoves" when {
    "given final state" should {
      "not have any moves" in {
        val state: State = Map(
          Position(Room.A.index, 1) -> Amphipod.A,
          Position(Room.A.index, 2) -> Amphipod.A,
          Position(Room.B.index, 1) -> Amphipod.B,
          Position(Room.B.index, 2) -> Amphipod.B,
          Position(Room.C.index, 1) -> Amphipod.C,
          Position(Room.C.index, 2) -> Amphipod.C,
          Position(Room.D.index, 1) -> Amphipod.D,
          Position(Room.D.index, 2) -> Amphipod.D
        )

        amphipods.possibleMoves(state) shouldBe empty
      }
    }

    "given a state with no correctly placed amphipods" should {
      "have 28 hallway moves" in {
        val state: State = Map(
          Position(Room.A.index, 1) -> Amphipod.D,
          Position(Room.B.index, 1) -> Amphipod.C,
          Position(Room.C.index, 1) -> Amphipod.B,
          Position(Room.D.index, 1) -> Amphipod.A
        )

        val moves = amphipods.possibleMoves(state)

        moves.size should be(28)
        forAll(moves) {
          move =>
            move.exists(_._1.y == amphipods.hallwayY) should be(true)
        }
      }
    }

    "given a state with one amphipod in hallway" should {
      "move amphipod to correct room" in {
        val state: State = Map(
          Position(0, 0) -> Amphipod.A
        )

        val moves = amphipods.possibleMoves(state)

        moves.size should be(1)

        moves.head.head._1 should be(Position(Room.A.index, 2))
      }
    }

    "given a state with one amphipod in invalid room" should {
      "move amphipod to correct room" in {
        val state: State = Map(
          Position(Room.A.index, 2) -> Amphipod.B
        )

        val moves = amphipods.possibleMoves(state)

        moves.size should be(1)

        moves.head.head._1 should be(Position(Room.B.index, 2))
      }
    }
  }

  "moveCost" should {
    "calculate energy cost correctly" when {
      "given a move between two rooms" in {
        val state1: State = Map(
          Position(Room.A.index, 2) -> Amphipod.B
        )
        val state2: State = Map(
          Position(Room.B.index, 2) -> Amphipod.B
        )

        val energyCost = amphipods.moveCost(state1, state2)

        energyCost should be(6 * Amphipod.B.energyPerMove)
      }

      "given a move from hallway to room" in {
        val state1: State = Map(
          Position(0, 0) -> Amphipod.B
        )
        val state2: State = Map(
          Position(Room.B.index, 2) -> Amphipod.B
        )

        val energyCost = amphipods.moveCost(state1, state2)

        energyCost should be(6 * Amphipod.B.energyPerMove)
      }
    }
  }
}
